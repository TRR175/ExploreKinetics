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

    type CombinationType =
        | Combine
        | Stack

        static member ofString str =
            match str with
            | "Combine" | "combine" -> Combine
            | "Stack" | "stack" -> Stack

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
        TrivialName         : string
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
        static member create id tn t0 t1a t2a t3a t4a t5a t1d t2d t3d t4d t5d = 
            {
                TranscriptIdentifier= id
                TrivialName         = tn
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
                "0"          => row.T0
                "1_A"      => row.T15_Acc
                "15_A"     => row.T15_Acc    
                "180_A"    => row.T180_Acc   
                "2880_A"   => row.T2880_Acc  
                "5760_A"   => row.T5760_Acc  
                "1_D"    => row.T1_DeAcc   
                "15_D"   => row.T15_DeAcc  
                "180_D"  => row.T180_DeAcc 
                "2880_D" => row.T2880_DeAcc
                "5760_D" => row.T5760_DeAcc
            ]

    type KineticControlRow = {
        TranscriptIdentifier: string
        TrivialName         : string 
        T0                  : float
        T180_Acc            : float
        T2880_Acc           : float
        T5760_Acc           : float
        T180_DeAcc          : float
        T2880_DeAcc         : float
        T5760_DeAcc         : float
    } with
        static member create id tn t0 t3a t4a t5a t3d t4d t5d = 
            {
                TranscriptIdentifier= id
                TrivialName         = tn
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
                "0"          => row.T0
                "180_A"    => row.T180_Acc   
                "2880_A"   => row.T2880_Acc  
                "5760_A"   => row.T5760_Acc  
                "180_D"  => row.T180_DeAcc 
                "2880_D" => row.T2880_DeAcc
                "5760_D" => row.T5760_DeAcc
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
        |> Frame.groupRowsBy "TrivialName"
        |> Frame.mapRows (fun ((tName:string),(id:string)) os ->
            (sprintf "[%s] %s" id (tName.Replace(";", "; "))) => 
                KineticControlRow.create
                    id
                    (tName.Replace(";", "; "))
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
        |> Frame.groupRowsBy "TrivialName"
        |> Frame.mapRows (fun ((tName:string),(id:string)) os ->
            (sprintf "[%s] %s" id (tName.Replace(";", "; "))) => 
                KineticDataRow.create
                    id 
                    (tName.Replace(";", "; "))
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
        |> Frame.groupRowsBy "TrivialName"
        |> Frame.mapRows (fun ((tName:string),(id:string)) os ->
            (sprintf "[%s] %s" id (tName.Replace(";", "; "))) => 
                KineticDataRow.create
                    id 
                    (tName.Replace(";", "; "))
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
        |> Frame.groupRowsBy "TrivialName"
        |> Frame.mapRows (fun ((tName:string),(id:string)) os ->
            (sprintf "[%s] %s" id (tName.Replace(";", "; "))) => 
                KineticDataRow.create
                    id 
                    (tName.Replace(";", "; "))
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

    let getControlDataForId id = 
        controlData
        |> Map.find id
        |> fun row -> id => (KineticControlRow.getPlotData row)

    let getFigureForIdsAndConditions (geneIDs : string []) (conditions: Condition []) (combinationType: CombinationType) (includeControl: bool) : GenericChart.Figure =

        let xAxis() = 
            Axis.LinearAxis.init(
                Title = "Sampled timepoint [[min]_[acclimation state]]",
                Ticks = StyleParam.TickOptions.Inside,
                Mirror = StyleParam.Mirror.AllTicks,
                Showline = true
            )
        
        let yAxis() = 
            Axis.LinearAxis.init(
                Title = "logFPKM",
                Ticks = StyleParam.TickOptions.Inside,
                Mirror = StyleParam.Mirror.AllTicks,
                Showline = true
            )

        let controlPlot id = 
            if includeControl then
                [|
                    getControlDataForId id
                    |> fun (id,data) ->
                        Chart.Spline(data,Width=3,Smoothing=0.3,ShowMarkers = true)
                        |> Chart.withTraceName (sprintf "[Contol]: %s" (id.Split("]").[0].Replace("[","")))
                |]
            else    
                [||]

        geneIDs
        |> Array.map (fun id ->
            conditions
            |> Array.map (fun condition ->
                getDataForIdAndCondition id condition
                |> fun (id,data) ->
                    Chart.Spline(data,Width=3,Smoothing=0.3,ShowMarkers = true)
                    |> Chart.withTraceName (sprintf "[%s]: %s" (Condition.toString condition) (id.Split("]").[0].Replace("[","")))
            )
            |> fun x -> Array.concat [x; controlPlot id]
            |> Chart.Combine
        )
        |> fun charts ->
            match combinationType with
            | Combine ->
                charts
                |> Chart.Combine
                |> Chart.withX_Axis (xAxis())
                |> Chart.withY_Axis (yAxis())
                |> Chart.withTitle "<b>Comparison plot</b>"
                |> GenericChart.mapLayout (fun l -> 
                    l?height <- 700
                    l?font <- Font.init (Family=StyleParam.FontFamily.Droid_Sans , Size = 18)
                    l?colorway <- ChartTemplates.ColorWays.plotly
                    l
                )
                |> GenericChart.toFigure
            | Stack ->
                let len = charts.Length
                charts
                |> Array.map ( fun c ->
                    c
                    |> Chart.withX_Axis (xAxis())
                    |> Chart.withY_Axis (yAxis())
                    |> GenericChart.mapLayout (fun l -> 
                        l?colorway <- ChartTemplates.ColorWays.plotly
                        l
                    )
                )
                |> Chart.SingleStack
                |> Chart.withTitle "<b>Comparison plot</b>"
                |> GenericChart.mapLayout (fun l -> 
                    l?height <- 200 * len
                    l?font <- Font.init (Family=StyleParam.FontFamily.Droid_Sans , Size = 18)
                    l?colorway <- ChartTemplates.ColorWays.plotly
                    l
                )
                |> GenericChart.toFigure

//Note that this layout uses css classes defined by Bulma (https://bulma.io/), which gets defined as a css dependency in the app section below.
open DashAppDomain

let testGraph = 
    Data.getFigureForIdsAndConditions
        [|"[AT3G12580] Q9LHA8; HSP70; ATHSP70"|]
        [|Condition.Cold; Condition.Heat; Condition.HighLight|]
        CombinationType.Combine
        false

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

let controlCheckbox =
    RadioItems.radioItems "controlCheckbox" [
        RadioItems.Options [
            RadioItemsOption.create "Yes" "Yes" false ""
            RadioItemsOption.create "No" "No" false ""
        ]
        RadioItems.Value "No"
    ] []

let combineCheckbox =
    RadioItems.radioItems "combineCheckbox" [
        RadioItems.Options [
            RadioItemsOption.create "Combine" "Combine" false ""
            RadioItemsOption.create "Stack" "Stack" false ""
        ]
        RadioItems.Value "Combine"
    ] []

let formControl labelText children = 
    Div.div [ClassName "field"] [
        Label.label [ClassName "label"] [str labelText]
        Div.div [ClassName "control"] children
    ]

let icon fas text =
    Span.span [ClassName "icon-text"] [
         Span.span [ClassName "icon-text"] [I.i [ClassName (sprintf "%s" fas)][]]
         Span.span [] [str text]
    ]


let layout = 
    Div.div [Id "layout"] [
        Section.section [ClassName "hero is-primary"] [
            Div.div [ClassName "hero-body"] [
                Div.div [ClassName "container"] [
                    Div.div [ClassName "media"] [
                        Div.div [ClassName "media-left"] [
                            Figure.figure [ClassName "image is-128x128"] [
                                Img.img [Custom ("src", box "img/TRR175.png")] []
                            ]
                        ]
                        Div.div [ClassName "media-content"] [
                            H1.h1 [ClassName "title"] [str "ExploreKinetics"]
                            H2.h2 [ClassName "subtitle"] [str "Explore the TRR175 core kinetics dataset!"]
                            H2.h2 [ClassName "subtitle"] [str "Use the chart controls on the left to plot the timeseries data of your gene(s) of choice compared across the conditions of choice."]
                        ]
                    ]
                ]
            ]
            Div.div [ClassName "hero-foot mb-4"] [
                Nav.nav [ClassName "tabs"] [
                    Div.div [ClassName "container"] [
                        Ul.ul [][
                            Li.li [] [A.a [Custom("href",box"https://github.com/TRR175/ExploreKinetics")] [icon "fas fa-code-branch" "Source code"]]
                            Li.li [] [A.a [Custom("href",box"mailto:muehlhaus@bio.uni-kl.de")] [icon "fas fa-user-friends" "Contact"]]
                            Li.li [] [A.a [Custom("href",box"https://twitter.com/SFB_TR175")] [icon "fab fa-twitter" "Get in touch"]]
                            Li.li [] [A.a [Custom("href",box"https://github.com/TRR175/ExploreKinetics/discussions")] [icon "fas fa-comments" "Suggest a feature"]]
                        ]
                    ]
                ]
            ]
        ]
        Div.div [ClassName "columns"; Id "mainColumns"] [
            Div.div [ClassName "column is-3 p-4 m-0 has-background-white-ter"; Id "plotParameters"] [
                Div.div [ClassName "container"] [ 
                    H1.h1 [ClassName "title pt-4 pb-4"] [str "plot parameters"]
                    formControl "Compare for the following transcript(s):" [idSelectionDropdown]
                    formControl "Across the following conditions:" [conditionSelectionDropdown]
                    formControl "Include control trace for each gene?" [controlCheckbox]
                    formControl "Combine gene plots via" [combineCheckbox] 
                    formControl "" [Button.button [ClassName "button is-primary"; Id "startbtn"] [str "Compare"]] 
                ]
            ]
            Div.div [ClassName "column is-9 graphColumn"] [
                Graph.graph "mainGraph" [Graph.Figure testGraph;] []
            ]
            
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
        (CallbackOutput.create("mainGraph","figure")),
        (
            fun (clicks:int64) (ids:JArray) (conditions:JArray) (includeControl:string) (combinationType:string) ->
                let ids = ids.ToObject<string[]>()
                let conditions = conditions.ToObject<string[]>()
                let includeControl = match includeControl with | "Yes" -> true | "No" -> false
                let combinationType = combinationType |> CombinationType.ofString
                Data.getFigureForIdsAndConditions ids (conditions |> Array.map Condition.ofString) combinationType includeControl
        ),
        State= [
            CallbackState.create("idSelection","value")
            CallbackState.create("conditionSelection","value")
            CallbackState.create("controlCheckbox","value")
            CallbackState.create("combineCheckbox","value")
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
        "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css" // register bulma as an external css dependency
        "main.css" // serve your custom css
    ]
    |> DashApp.appendScripts ["https://kit.fontawesome.com/0d3e0ea7a6.js"] 
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