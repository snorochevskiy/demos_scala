package snorochevskiy.cats.effects

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.junit.WireMockRule
import org.scalatest.funsuite.AsyncFunSuiteLike

class HttpClientTest extends AsyncFunSuiteLike {

  test("get") {
    val wireMockRule = new WireMockRule(8089)

    wireMockRule.stubFor(get(urlEqualTo("/test-path"))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "text/plain")
        .withBody("text response")))

    wireMockRule.start()

    val io = HttpClient.get("http://localhost:8089/test-path")

    val res = io.unsafeRunSync()

    assert(res === "text response")
  }

  test("post") {
    val wireMockRule = new WireMockRule(8089)

    wireMockRule.stubFor(post(urlEqualTo("/test-path"))
      .withRequestBody(equalTo("test-payload"))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "text/plain")
        .withBody("text response")))

    wireMockRule.start()

    val io = HttpClient.post("http://localhost:8089/test-path", "test-payload")

    val res = io.unsafeRunSync()

    assert(res === "text response")
  }
}
