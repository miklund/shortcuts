module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma
open Fulma.FontAwesome
open Fable.Import.React
open Fulma.FontAwesome


type Model = Counter option

type Msg =
| Increment
| Decrement
| SaveForm
| Init of Result<Counter, exn>



let init () : Model * Cmd<Msg> =
    let model = None
    let cmd =
        Cmd.ofPromise
            (fetchAs<int> "/api/init")
            []
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let model' =
        match model,  msg with
        | _, SaveForm -> Some 12
        | Some x, Increment -> Some (x + 1)
        | Some x, Decrement -> Some (x - 1)
        | None, Init (Ok x) -> Some x
        | _ -> None
    model', Cmd.none

// let safeComponents =
//     let intersperse sep ls =
//         List.foldBack (fun x -> function
//             | [] -> [x]
//             | xs -> x::sep::xs) ls []

//     let components =
//         [
//             "Saturn", "https://saturnframework.github.io/docs/"
//             "Fable", "http://fable.io"
//             "Elmish", "https://elmish.github.io/elmish/"
//             "Fulma", "https://mangelmaxime.github.io/Fulma"
//         ]
//         |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
//         |> intersperse (str ", ")
//         |> span [ ]

//     p [ ]
//         [ strong [] [ str "SAFE Template" ]
//           str " powered by: "
//           components ]

// let show = function
// | Some x -> string x
// | None -> "Loading..."

// let button txt onClick =
//     Button.button
//         [ Button.IsFullWidth
//           Button.Color IsPrimary
//           Button.OnClick onClick ]
//         [ str txt ]

// let view (model : Model) (dispatch : Msg -> unit) =
//     div []
//         [ Navbar.navbar [ Navbar.Color IsPrimary ]
//             [ Navbar.Item.div [ ]
//                 [ Heading.h2 [ ]
//                     [ str "SAFE Template" ] ] ]

//           Container.container []
//               [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                     [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
//                 Columns.columns []
//                     [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
//                       Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ]

//           Footer.footer [ ]
//                 [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                     [ safeComponents ] ] ]


type FormControlViewModel =
    { form : FormControl
    ; label : string
    ; placeholder : string
    ; icon : Fa.I.FontAwesomeIcons
    ; helpMessage : string
    }

type Validated = | NotValidated | ValidationSuccess | ValidationError 

let viewFormControl (model : FormControlViewModel) =
    let inputColor = 
        match model.form.validation with
        | Some v when v.isValid -> Input.Color IsSuccess
        | Some _ -> Input.Color IsDanger
        | None -> Input.Color IsGrey

    let helpColor =
        match model.form.validation with
        | Some v when v.isValid -> IsSuccess
        | Some _ -> IsDanger
        | None -> IsInfo

    let helpMessage =
        match model.form.validation with
        | Some v -> v.message
        | None -> model.helpMessage 
    
    let validationIcon =
        match model.form.validation with
        | Some v when v.isValid -> [ Icon.faIcon [ Icon.Size IsSmall ; Icon.IsRight ] [ Fa.icon Fa.I.Check ] ]
        | Some _ -> [ Icon.faIcon [ Icon.Size IsSmall ; Icon.IsRight ] [ Fa.icon Fa.I.Warning ] ]
        | None -> []

    Field.div []
      [ Label.label [] [ str model.label ]
      ; Control.div [ Control.HasIconLeft ; Control.HasIconRight ]
        (
        [ Input.text [ Input.Placeholder model.placeholder ; inputColor ]
          // left icon
        ; Icon.faIcon [ Icon.Size IsSmall ; Icon.IsLeft ] [ Fa.icon model.icon ]
        ] 
          // right icon
        @ validationIcon
        )
      // info message or validation message
      ; Help.help [ Help.Color helpColor ] [ str helpMessage ]
      ]


let viewShortcutsTableRow (model : UrlRecord) =
    tr [ ]
        // short
         [ td [ ] [ a [ Href model.originalUrl ] [ str model.shortUrl ] ]
         // count
         ; td [ ] [ str (sprintf "%d" model.count) ]
         // edit
         ; td [ ] [ Button.button [ ] [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.Edit ] ] ] 
         // remove
         ; td [ ] [ Button.button [ ] [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.Remove ] ] ] 
         ]

let view (model : Model) (dispatch : Msg -> unit) =
    let submitForm = fun _ -> dispatch SaveForm

    Container.container []
        [ h1 [] [ Heading.h1 [] [ str "stratiteq.link" ] ]
        ; p [] [ str "Add a link below" ]
        ; form [ ]
          [ // Original Url
            viewFormControl 
                { form =
                  { value = ""
                  ; validation = None
                  }
                ; label = "Original URL"
                ; placeholder = "http://"
                ; icon = Fa.I.Link
                ; helpMessage = "Please enter the full URL you're linking to"
                }
          ; // Short Url
            viewFormControl 
                { form =
                  { value = ""
                  ; validation = None
                  }
                ; label = "Short URL"
                ; placeholder = "short-url"
                ; icon = Fa.I.ExternalLink
                ; helpMessage = "Enter the short-url part of https://stratiteq.link/short-url"
                }            
          ; // Submit button
            Field.div []
              [ Control.div []
                  [ Button.button [ Button.Color IsPrimary ; Button.OnClick submitForm  ] [ str "Submit" ]
                  ]
              ]
          ]
        ; h2 [] [ Heading.h2 [] [ str "Shortcuts" ] ]
        ; p [] [ str "All of your registered short urls" ]
        ; Table.table [ Table.IsBordered ; Table.IsFullWidth ; Table.IsStriped; Table.IsHoverable ]
            [ thead [ ]
                [ tr [ ]
                    [ th [ ] [ str "Short" ]
                    ; th [ ] [ str "Visits" ]
                    ; th [ ] [ str "Edit" ] 
                    ; th [ ] [ str "Delete" ] 
                    ]
                ]
            ; tbody [ ]
                [ viewShortcutsTableRow
                    { id = None
                    ; originalUrl = ""
                    ; shortUrl = "short"
                    ; count = 1
                    }
                ; viewShortcutsTableRow
                    { id = None
                    ; originalUrl = ""
                    ; shortUrl = "short"
                    ; count = 2
                    }
                ; viewShortcutsTableRow
                    { id = None
                    ; originalUrl = ""
                    ; shortUrl = "short"
                    ; count = 3
                    }
                ]
            ]
        ]
    


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
