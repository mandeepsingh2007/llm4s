package org.llm4s.llmconnect.provider

import com.sun.net.httpserver.{ HttpExchange, HttpServer }
import org.llm4s.error.{ AuthenticationError, RateLimitError, ServiceError, ValidationError }
import org.llm4s.llmconnect.config.MistralConfig
import org.llm4s.llmconnect.model.{ CompletionOptions, Conversation, UserMessage }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

class MistralClientSpec extends AnyFlatSpec with Matchers {

  private def withServer(handler: HttpExchange => Unit)(test: String => Any): Unit = {
    val server = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
    server.createContext("/v1/chat/completions", exchange => handler(exchange))
    server.start()

    val baseUrl = s"http://localhost:${server.getAddress.getPort}"

    try
      test(baseUrl)
    finally
      server.stop(0)
  }

  private def conversation: Conversation = Conversation(Seq(UserMessage("hello")))

  private def config(baseUrl: String): MistralConfig =
    MistralConfig(
      apiKey = "test-key",
      model = "mistral-small-latest",
      baseUrl = baseUrl,
      contextWindow = 128000,
      reserveCompletion = 4096
    )

  "MistralClient.complete" should "parse a successful OpenAI-compatible response" in withServer { exchange =>
    val body =
      """{
        |  "id": "cmpl-abc123",
        |  "object": "chat.completion",
        |  "created": 1700000000,
        |  "model": "mistral-small-latest",
        |  "choices": [
        |    {
        |      "index": 0,
        |      "message": {
        |        "role": "assistant",
        |        "content": "Hello! How can I help you?"
        |      },
        |      "finish_reason": "stop"
        |    }
        |  ],
        |  "usage": {
        |    "prompt_tokens": 10,
        |    "completion_tokens": 8,
        |    "total_tokens": 18
        |  }
        |}""".stripMargin

    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(200, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isRight shouldBe true

    val completion = result.toOption.get
    completion.content shouldBe "Hello! How can I help you?"
    completion.id shouldBe "cmpl-abc123"
    completion.usage.isDefined shouldBe true
    completion.usage.get.promptTokens shouldBe 10
    completion.usage.get.completionTokens shouldBe 8
  }

  it should "handle response with whitespace in content" in withServer { exchange =>
    val body =
      """{
        |  "id": "cmpl-xyz",
        |  "created": 1700000000,
        |  "model": "mistral-small-latest",
        |  "choices": [
        |    {
        |      "message": {
        |        "role": "assistant",
        |        "content": "  Hello world  "
        |      },
        |      "finish_reason": "stop"
        |    }
        |  ],
        |  "usage": {
        |    "prompt_tokens": 5,
        |    "completion_tokens": 3,
        |    "total_tokens": 8
        |  }
        |}""".stripMargin

    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(200, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isRight shouldBe true

    val completion = result.toOption.get
    completion.content shouldBe "Hello world"
    completion.usage.isDefined shouldBe true
    completion.usage.get.promptTokens shouldBe 5
    completion.usage.get.completionTokens shouldBe 3
  }

  it should "fail with ValidationError when required text is missing" in withServer { exchange =>
    val body =
      """{
        |  "id": "cmpl-empty",
        |  "created": 1700000000,
        |  "model": "mistral-small-latest",
        |  "choices": [
        |    {
        |      "message": {
        |        "role": "assistant",
        |        "content": ""
        |      },
        |      "finish_reason": "stop"
        |    }
        |  ]
        |}""".stripMargin

    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(200, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[ValidationError]
    result.left.toOption.get.message should include("Missing required text")
  }

  it should "map HTTP 401 to AuthenticationError" in withServer { exchange =>
    val body  = """{ "message": "Unauthorized" }"""
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(401, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[AuthenticationError]
  }

  it should "map HTTP 429 to RateLimitError" in withServer { exchange =>
    val body  = """{ "message": "Rate limit exceeded" }"""
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(429, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[RateLimitError]
  }

  it should "map HTTP 5xx to ServiceError" in withServer { exchange =>
    val body  = """{ "message": "Internal server error" }"""
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(500, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isLeft shouldBe true
    val err = result.left.toOption.get
    err shouldBe a[ServiceError]
    err.context("httpStatus") shouldBe "500"
  }

  it should "parse nested error.message format" in withServer { exchange =>
    val body  = """{ "error": { "message": "Bad request details" } }"""
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(400, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  } { baseUrl =>
    val client = new MistralClient(config(baseUrl))

    val result = client.complete(conversation, CompletionOptions())
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[ValidationError]
    result.left.toOption.get.message should include("Bad request details")
  }
}
