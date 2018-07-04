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
open System.Net
open Fable.Import.Browser


type Model = ViewModel

type Msg =
| OriginalUrlChange of string
| ShortUrlChange of string
| SaveForm
| EditUrl of int
| DeleteUrl of int
| Init of Result<ViewModel, exn>


let init () : Model * Cmd<Msg> =
    let model = 
      { notification = None
      ; form =
          { id = None
          ; originalUrl = defaultFormControl
          ; shortUrl = defaultFormControl
          }
      ; table = 
        [ (1, { originalUrl = ""
          ; shortUrl = "short"
          ; count = 1
          })
        ; (2, { originalUrl = ""
          ; shortUrl = "short"
          ; count = 2
          })
        ; (3, { originalUrl = ""
          ; shortUrl = "short"
          ; count = 3
          })
        ] |> Map.ofList
      }
    // let's do this later 

    // let cmd =
    //     Cmd.ofPromise
    //         (fetchAs<ViewModel> "/api/init")
    //         []
    //         (Ok >> Init)
    //         (Error >> Init)
    model, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =

    let validate (control : FormControl) validations =
        let pickValidationResult prev current =
            match prev, current with
            // if not validated, new state is always right
            | { validation = None }, current -> current
            // if failed state, always pick first failed state
            | { validation = Some prev }, _ as control when not prev.isValid -> fst control
            // default is to pick the new state
            | _, current -> current

        validations
        |> List.map (fun validation -> validation control)
        |> List.fold pickValidationResult control

    let updateOriginalUrl model input =
        { model with form = { model.form with originalUrl = { value = input ; validation = None } } }

    let validateOriginalUrl model =
        let originalUrl =
            // maxLength 2047 because IE11 & Edge
            [ validateRequired ; validateAbsoluteUrl 10 ; validateLength 2047 ]
            |> validate model.form.originalUrl
        { model with form = { model.form with originalUrl = originalUrl } }

    let updateShortUrl model input =
        { model with form = { model.form with shortUrl = { value = input ; validation = None } } }

    let validateShortUrl model =
        let shortUrl =
            [ validateRequired ; validateUrlPart ; validateLength 64 ]
            |> validate model.form.shortUrl
        { model with form = { model.form with shortUrl = shortUrl } }

    let isValid  = function
    | { form = { originalUrl = { validation = Some { isValid = true } } ; shortUrl = { validation = Some { isValid = true } } } } -> true
    | _ -> false

    let model' =
        match model,  msg with
        | model, OriginalUrlChange input -> (updateOriginalUrl model input) |> validateOriginalUrl
        | model, ShortUrlChange input -> (updateShortUrl model input) |> validateShortUrl   
        // take the editable url shortcut and put in the form
        | model, EditUrl id ->
            { model with
                  form = 
                  { id = Some id
                  ; originalUrl = { value = model.table.[id].originalUrl ; validation = None }
                  ; shortUrl = { value = model.table.[id].shortUrl ; validation = None }
                  }
            }
        | model, DeleteUrl id ->
            { model with table = Map.remove id model.table }
        // to save form, both fields must be valid
        | model, SaveForm when model |> validateOriginalUrl |> validateShortUrl |> isValid -> 
            { notification = None
            ; form =
              // reset
              { id = None
              ; originalUrl = defaultFormControl
              ; shortUrl = defaultFormControl
              }
          ; table = 
              // append new item
              model.table.Add (99, { originalUrl = model.form.originalUrl.value ; shortUrl = model.form.shortUrl.value ; count = 0 })
          }
        // save form but fields are not valid
        | model, SaveForm -> { model with notification = Some { message = "Please review the validation messages" ; status = Error } }
        | _ -> failwith "Unknown message yo, man!"

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
    ; maxLength : float
    ; icon : Fa.I.FontAwesomeIcons
    ; helpMessage : string
    }

type Validated = | NotValidated | ValidationSuccess | ValidationError 

let viewFormControl (model : FormControlViewModel) (inputChange : FormEvent -> unit) =
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
        | Some { isValid = false } as v -> v.Value.message
        | _ -> model.helpMessage 
    
    let validationIcon =
        match model.form.validation with
        | Some v when v.isValid -> [ Icon.faIcon [ Icon.Size IsSmall ; Icon.IsRight ] [ Fa.icon Fa.I.Check ] ]
        | Some _ -> [ Icon.faIcon [ Icon.Size IsSmall ; Icon.IsRight ] [ Fa.icon Fa.I.Warning ] ]
        | None -> []

    Field.div []
      [ Label.label [] [ str model.label ]
      ; Control.div [ Control.HasIconLeft ; Control.HasIconRight ]
        (
        [ Input.text [ Input.Props [ MaxLength model.maxLength ] ; Input.DefaultValue model.form.value ; Input.Placeholder model.placeholder ; inputColor ; Input.OnChange inputChange ]
          // left icon
        ; Icon.faIcon [ Icon.Size IsSmall ; Icon.IsLeft ] [ Fa.icon model.icon ]
        ] 
          // right icon
        @ validationIcon
        )
      // info message or validation message
      ; Help.help [ Help.Color helpColor ] [ str helpMessage ]
      ]


let viewShortcutsTableRow (id : int) (model : UrlRecord) (dispatch : Msg -> unit) =
    tr [ ]
        // short
         [ td [ ] [ a [ Href model.originalUrl ] [ str model.shortUrl ] ]
         // count
         ; td [ ] [ str (sprintf "%d" model.count) ]
         // edit
         ; td [ ] [ Button.button [ Button.OnClick (fun _ -> dispatch (EditUrl id) ) ] [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.Edit ] ] ] 
         // remove
         ; td [ ] [ Button.button [ Button.OnClick (fun _ -> dispatch (DeleteUrl id) ) ] [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.Remove ] ] ] 
         ]

let viewNotification = function
    | None -> []
    | Some notification when notification.status = Ok -> [ Notification.notification [ Notification.Color IsSuccess ] [str notification.message] ]
    | Some notification when notification.status = Error -> [ Notification.notification [ Notification.Color IsDanger ] [str notification.message] ]
    | Some notification when notification.status = Info -> [ Notification.notification [ Notification.Color IsInfo ] [str notification.message] ]
    | _ -> failwith "Unknown type of notifiaction"

let view (model : Model) (dispatch : Msg -> unit) =
    let submitForm = fun _ -> dispatch SaveForm

    Container.container []
        [ h1 [] [ Heading.h1 [] [ str "stratiteq.link" ] ]
        ; div [] (viewNotification model.notification)
        ; p [] [ str "Add a link below" ]
        ; div [ ]
          [ // Original Url
             viewFormControl 
                { form = model.form.originalUrl
                ; label = "Original URL"
                ; placeholder = "http://"
                ; maxLength = 65536.
                ; icon = Fa.I.Link
                ; helpMessage = 
                    match model.form.originalUrl.validation with 
                    | None | Some { isValid = true } -> "Please enter the full URL you're linking to"
                    | Some v -> v.message
                }
                (fun e -> dispatch (OriginalUrlChange e.Value))
          ; // Short Url
            viewFormControl 
                { form = model.form.shortUrl
                ; label = "Short URL"
                ; placeholder = "short-url"
                ; maxLength = 64.
                ; icon = Fa.I.ExternalLink
                ; helpMessage = 
                    match model.form.shortUrl.validation with 
                    | None | Some { isValid = true } -> "Enter the short-url part of https://stratiteq.link/short-url"
                    | Some v -> v.message
                }
                (fun e -> dispatch (ShortUrlChange e.Value))   
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
            // TODO too messy
            ; tbody [ ] (model.table |> Map.toList |> List.map (fun (id, record) -> viewShortcutsTableRow id record dispatch) )
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
