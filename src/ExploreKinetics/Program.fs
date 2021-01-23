module Dash.NET.POC.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.ModelBinding

open Dash.NET
open Plotly.NET

//----------------------------------------------------------------------------------------------------
//============================================== LAYOUT ==============================================
//----------------------------------------------------------------------------------------------------

//The layout describes the components that Dash will render for you. 
open Dash.NET.HTML // this namespace contains the standard html copmponents, such as div, h1, etc.
open Dash.NET.DCC  // this namespace contains the dash core components, the heart of your Dash.NET app.

open HTMLPropTypes
open ComponentPropTypes

//Note that this layout uses css classes defined by Bulma (https://bulma.io/), which gets defined as a css dependency in the app section below.
let layout = 
    Div.div [] [
        Section.section [ClassName "hero is-primary"] [
            Div.div [ClassName "hero-body"] [
                Div.div [ClassName "container"] [
                    H1.h1 [ClassName "title"] [str "ExploreKinetics"]
                    H2.h2 [ClassName "subtitle"] [str "Explore the TRR175 core kinetics dataset!"]
                ]
            ]
        ]
    ]

//----------------------------------------------------------------------------------------------------
//============================================= Callbacks ============================================
//----------------------------------------------------------------------------------------------------




///--------------------------------------------------------------------------------------------------
//============================================= The App ==============================================
//----------------------------------------------------------------------------------------------------

//The 'DashApp' type is your central DashApp that contains all settings, configs, the layout, styles, 
//scripts, etc. that makes up your Dash.NET app. 

let dashApp =
    DashApp.initDefault() // create a Dash.NET app with default settings
    |> DashApp.withLayout layout // register the layout defined above.
    |> DashApp.appendCSSLinks [ 
        "main.css" // serve your custom css
        "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css" // register bulma as an external css dependency
    ]

// The things below are Giraffe/ASP:NetCore specific and will likely be abstracted in the future.

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.EnvironmentName with
    | "Development" -> app.UseDeveloperExceptionPage()
    | _ -> app.UseGiraffeErrorHandler(errorHandler))
        .UseHttpsRedirection()
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(DashApp.toHttpHandler dashApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Debug)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0