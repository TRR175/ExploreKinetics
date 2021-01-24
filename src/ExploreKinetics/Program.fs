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
open Deedle

open HTMLPropTypes
open ComponentPropTypes

module DashAppDomain = 

    type Condition =
        | Heat
        | Cold
        | HighLight

        static member ofString str = 
            match str with
            | "heat" | "Heat" -> Heat
            | "cold" | "Cold" -> Cold
            | "high light" | "High Light" | "High light"| "HighLight" -> HighLight
            | _ -> failwithf "cannot parste string %s to condition" str

        static member toString condition =
            match condition with
            | Heat      -> "Heat"
            | Cold      -> "Cold"
            | HighLight -> "HighLight"

    type KineticDataRow = {
        TranscriptIdentifier: string
        T0                  : float
        T1_Acc              : float
        T15_Acc             : float
        T180_Acc            : float
        T2880_Acc           : float
        T5760_Acc           : float
        T1_DeAcc            : float
        T15_DeAcc           : float
        T180_DeAcc          : float
        T2880_DeAcc         : float
        T5760_DeAcc         : float
    } with
        static member create id t0 t1a t2a t3a t4a t5a t1d t2d t3d t4d t5d = 
            {
                TranscriptIdentifier= id
                T0                  = t0
                T1_Acc              = t1a
                T15_Acc             = t2a
                T180_Acc            = t3a
                T2880_Acc           = t4a
                T5760_Acc           = t5a
                T1_DeAcc            = t1d
                T15_DeAcc           = t2d
                T180_DeAcc          = t3d
                T2880_DeAcc         = t4d
                T5760_DeAcc         = t5d
            }

        static member getPlotData (row: KineticDataRow) =
            [
                "0min"          => row.T0
                "1min_Acc"      => row.T15_Acc
                "15min_Acc"     => row.T15_Acc    
                "180min_Acc"    => row.T180_Acc   
                "2880min_Acc"   => row.T2880_Acc  
                "5760min_Acc"   => row.T5760_Acc  
                "1min_DeAcc"    => row.T1_DeAcc   
                "15min_DeAcc"   => row.T15_DeAcc  
                "180min_DeAcc"  => row.T180_DeAcc 
                "2880min_DeAcc" => row.T2880_DeAcc
                "5760min_DeAcc" => row.T5760_DeAcc
            ]

    type KineticControlRow = {
        TranscriptIdentifier: string
        T0                  : float
        T180_Acc            : float
        T2880_Acc           : float
        T5760_Acc           : float
        T180_DeAcc          : float
        T2880_DeAcc         : float
        T5760_DeAcc         : float
    } with
        static member create id t0 t3a t4a t5a t3d t4d t5d = 
            {
                TranscriptIdentifier= id
                T0                  = t0
                T180_Acc            = t3a
                T2880_Acc           = t4a
                T5760_Acc           = t5a
                T180_DeAcc          = t3d
                T2880_DeAcc         = t4d
                T5760_DeAcc         = t5d
            }

        static member getPlotData (row: KineticControlRow) =
            [
                "0min"          => row.T0
                "180min_Acc"    => row.T180_Acc   
                "2880min_Acc"   => row.T2880_Acc  
                "5760min_Acc"   => row.T5760_Acc  
                "180min_DeAcc"  => row.T180_DeAcc 
                "2880min_DeAcc" => row.T2880_DeAcc
                "5760min_DeAcc" => row.T5760_DeAcc
            ]

module Data =

    open DashAppDomain
    open System.IO
    open System.Reflection

    let assembly = Assembly.GetExecutingAssembly()

    let controlData : Map<string,KineticControlRow> =
        
        //Control header:
            //Cont_t_0_d_FALSE_mean
            //Cont_t_180_d_FALSE_mean
            //Cont_t_2880_d_FALSE_mean
            //Cont_t_5760_d_FALSE_mean
            //Cont_t_180_d_TRUE_mean
            //Cont_t_2880_d_TRUE_mean
            //Cont_t_5760_d_TRUE_mean
        
        let ressourcePath = assembly.GetManifestResourceNames() |> Seq.find (fun x -> x.EndsWith("Control_LogFPKM.csv"))
        let stream = assembly.GetManifestResourceStream(ressourcePath)
        Frame.ReadCsv(stream)
        |> Frame.indexRows "TranscriptIdentifier"
        |> Frame.mapRows (fun rk os ->
            rk => 
            KineticControlRow.create
                rk
                (os.GetAs<float>("Cont_t_0_d_FALSE_mean"))
                (os.GetAs<float>("Cont_t_180_d_FALSE_mean"))
                (os.GetAs<float>("Cont_t_2880_d_FALSE_mean"))
                (os.GetAs<float>("Cont_t_5760_d_FALSE_mean"))
                (os.GetAs<float>("Cont_t_180_d_TRUE_mean"))
                (os.GetAs<float>("Cont_t_2880_d_TRUE_mean"))
                (os.GetAs<float>("Cont_t_5760_d_TRUE_mean"))
        )
        |> Series.values
        |> Map.ofSeq

    let heatData : Map<string,KineticDataRow> = 

        //Heat header:
            //Cont_t_0_d_FALSE_mean
            //Heat_t_1_d_FALSE_mean
            //Heat_t_15_d_FALSE_mean
            //Heat_t_180_d_FALSE_mean
            //Heat_t_2880_d_FALSE_mean
            //Heat_t_5760_d_FALSE_mean
            //Heat_t_1_d_FALSE_mean
            //Heat_t_15_d_FALSE_mean
            //Heat_t_180_d_TRUE_mean
            //Heat_t_2880_d_TRUE_mean
            //Heat_t_5760_d_TRUE_mean
        
        let ressourcePath = assembly.GetManifestResourceNames() |> Seq.find (fun x -> x.EndsWith("Heat_LogFPKM.csv"))
        let stream = assembly.GetManifestResourceStream(ressourcePath)
        Frame.ReadCsv(stream)
        |> Frame.indexRows "TranscriptIdentifier"
        |> Frame.mapRows (fun rk os ->
            rk => 
            KineticDataRow.create
                rk
                (os.GetAs<float>("Cont_t_0_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_1_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_15_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_180_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_2880_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_5760_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_1_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_15_d_FALSE_mean"))
                (os.GetAs<float>("Heat_t_180_d_TRUE_mean"))
                (os.GetAs<float>("Heat_t_2880_d_TRUE_mean"))
                (os.GetAs<float>("Heat_t_5760_d_TRUE_mean"))
        )
        |> Series.values
        |> Map.ofSeq

    let coldData : Map<string,KineticDataRow> = 
        
        //Heat header:
            //Cont_t_0_d_FALSE_mean
            //Cold_t_1_d_FALSE_mean
            //Cold_t_15_d_FALSE_mean
            //Cold_t_180_d_FALSE_mean
            //Cold_t_2880_d_FALSE_mean
            //Cold_t_5760_d_FALSE_mean
            //Cold_t_1_d_TRUE_mean
            //Cold_t_15_d_TRUE_mean
            //Cold_t_180_d_TRUE_mean
            //Cold_t_2880_d_TRUE_mean
            //Cold_t_5760_d_TRUE_mean

        let ressourcePath = assembly.GetManifestResourceNames() |> Seq.find (fun x -> x.EndsWith("Cold_LogFPKM.csv"))
        let stream = assembly.GetManifestResourceStream(ressourcePath)
        Frame.ReadCsv(stream)
        |> Frame.indexRows "TranscriptIdentifier"
        |> Frame.mapRows (fun rk os ->
            rk => 
            KineticDataRow.create
                rk
                (os.GetAs<float>("Cont_t_0_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_1_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_15_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_180_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_2880_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_5760_d_FALSE_mean"))
                (os.GetAs<float>("Cold_t_1_d_TRUE_mean"))
                (os.GetAs<float>("Cold_t_15_d_TRUE_mean"))
                (os.GetAs<float>("Cold_t_180_d_TRUE_mean"))
                (os.GetAs<float>("Cold_t_2880_d_TRUE_mean"))
                (os.GetAs<float>("Cold_t_5760_d_TRUE_mean"))
        )
        |> Series.values
        |> Map.ofSeq

    let highLightData : Map<string,KineticDataRow> = 
        
        //Heat header:
            //Cont_t_0_d_FALSE_mean
            //Hlig_t_1_d_FALSE_mean
            //Hlig_t_15_d_FALSE_mean
            //Hlig_t_180_d_FALSE_mean
            //Hlig_t_2880_d_FALSE_mean
            //Hlig_t_5760_d_FALSE_mean
            //Hlig_t_1_d_TRUE_mean
            //Hlig_t_15_d_TRUE_mean
            //Hlig_t_180_d_TRUE_mean
            //Hlig_t_2880_d_TRUE_mean
            //Hlig_t_5760_d_TRUE_mean


        let ressourcePath = assembly.GetManifestResourceNames() |> Seq.find (fun x -> x.EndsWith("HighLight_LogFPKM.csv"))
        let stream = assembly.GetManifestResourceStream(ressourcePath)
        Frame.ReadCsv(stream)
        |> Frame.indexRows "TranscriptIdentifier"
        |> Frame.mapRows (fun rk os ->
            rk => 
            KineticDataRow.create
                rk
                (os.GetAs<float>("Cont_t_0_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_1_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_15_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_180_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_2880_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_5760_d_FALSE_mean"))
                (os.GetAs<float>("Hlig_t_1_d_TRUE_mean"))
                (os.GetAs<float>("Hlig_t_15_d_TRUE_mean"))
                (os.GetAs<float>("Hlig_t_180_d_TRUE_mean"))
                (os.GetAs<float>("Hlig_t_2880_d_TRUE_mean"))
                (os.GetAs<float>("Hlig_t_5760_d_TRUE_mean"))
        )
        |> Series.values
        |> Map.ofSeq

    let transcriptIds = 
        set [
            yield! controlData |> Seq.map (fun kv -> kv.Key)
            yield! heatData |> Seq.map (fun kv -> kv.Key)
            yield! coldData |> Seq.map (fun kv -> kv.Key)
            yield! highLightData |> Seq.map (fun kv -> kv.Key)
        ]

    let getDataForCondition (condition: Condition) = 
        match condition with
        | Heat      -> heatData
        | Cold      -> coldData
        | HighLight -> highLightData
    
    let getDataForIdAndCondition id condition = 
        condition
        |> getDataForCondition
        |> Map.find id
        |> fun row -> id => (KineticDataRow.getPlotData row)

    let getFigureForIdsAndConditions (geneIDs : string []) (conditions: Condition []) : GenericChart.Figure =
        geneIDs
        |> Array.map (fun id ->
            conditions
            |> Array.map (fun condition ->
                getDataForIdAndCondition id condition
                |> fun (id,data) ->
                    Chart.Line data
                    |> Chart.withTraceName (sprintf "[%s]: %s" (Condition.toString condition) id)
            )
            |> Chart.Combine
        )
        |> Chart.Combine
        |> GenericChart.toFigure

//Note that this layout uses css classes defined by Bulma (https://bulma.io/), which gets defined as a css dependency in the app section below.
open DashAppDomain

let testGraph = 
    Data.getFigureForIdsAndConditions
        [|"AT1G01010"; "AT1G01020"|]
        [|Condition.Cold; Condition.Heat; Condition.HighLight|]

let idSelectionDropdown = 
    Dropdown.dropdown "idSelection" [
        Dropdown.Multi true
        Dropdown.Placeholder "Start typing to search a transcript identifier ..."
        Dropdown.Options (
            Data.transcriptIds
            |> Seq.map (fun id -> DropdownOption.create id id false id)
        )
    ] []

let conditionSelectionDropdown =
    Dropdown.dropdown "conditionSelection" [
        Dropdown.Multi true
        Dropdown.Options (
            [
                Condition.Cold |> Condition.toString 
                Condition.Heat |> Condition.toString 
                Condition.HighLight |> Condition.toString 
            ]
            |> List.map (fun c -> DropdownOption.create c c false c)
        )
    ] []

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
        Section.section [ClassName "section"] [
            H2.h2 [] [str "Compare for the following transcript(s)"]
            idSelectionDropdown
            H2.h2 [] [str "Across the following conditions:"]
            conditionSelectionDropdown
            Button.button [ClassName "button is-primary"; Id "startbtn"] [str "Compare"]
            Br.br [] []
            Graph.graph "test" [Graph.Figure testGraph] []
        ]
    ]

//----------------------------------------------------------------------------------------------------
//============================================= Callbacks ============================================
//----------------------------------------------------------------------------------------------------

open Newtonsoft.Json
open Newtonsoft.Json.Linq

let comparisonCallback = 
    Callback(
        [CallbackInput.create("startbtn","n_clicks")],
        (CallbackOutput.create("test","figure")),
        (
            fun (clicks:int64) (ids:JArray) (conditions:JArray)->
                let ids = ids.ToObject<string[]>()
                let conditions = conditions.ToObject<string[]>()
                Data.getFigureForIdsAndConditions ids (conditions |> Array.map Condition.ofString)
        ),
        State= [
            CallbackState.create("idSelection","value")
            CallbackState.create("conditionSelection","value")
        ]
    )


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
    |> DashApp.addCallback comparisonCallback

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