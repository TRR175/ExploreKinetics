module Tests

open System
open System.IO
open System.Net
open System.Net.Http
open Xunit
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection

// ---------------------------------
// Helper functions (extend as you need)
// ---------------------------------

let createHost() =
    WebHostBuilder()
        .UseContentRoot(Directory.GetCurrentDirectory())
        .Configure(Action<IApplicationBuilder> ExploreKinetics.App.configureApp)
        .ConfigureServices(Action<IServiceCollection> ExploreKinetics.App.configureServices)

let runTask task =
    task
    |> Async.AwaitTask
    |> Async.RunSynchronously

let httpGet (path : string) (client : HttpClient) =
    path
    |> client.GetAsync
    |> runTask

let isStatus (code : HttpStatusCode) (response : HttpResponseMessage) =
    Assert.Equal(code, response.StatusCode)
    response

let ensureSuccess (response : HttpResponseMessage) =
    if not response.IsSuccessStatusCode
    then response.Content.ReadAsStringAsync() |> runTask |> failwithf "%A"
    else response

let readText (response : HttpResponseMessage) =
    response.Content.ReadAsStringAsync()
    |> runTask

let shouldEqual expected actual =
    Assert.Equal(expected, actual)

let shouldContain (expected : string) (actual : string) =
    Assert.True(actual.Contains expected)

// ---------------------------------
// Tests
// ---------------------------------
[<Fact>]
let ``Route / contains "Dash"`` () =
    use server = new TestServer(createHost())
    use client = server.CreateClient()
    client
    |> httpGet "/"
    |> ensureSuccess
    |> readText
    |> shouldContain "Hello world, from Giraffe!"