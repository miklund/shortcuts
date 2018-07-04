module Shared

type Counter = int

type Validation =
    { isValid : bool
    ; message : string
    }

type FormControl =
    { value : string
    ; validation : Validation Option
    }

let validateLength maxLength = function
    | { value = inputValue } as control when inputValue.Length > maxLength -> { control with validation = Some { isValid = false ; message = sprintf "Maximum number of characters allowed are %d" maxLength } }
    | control -> { control with validation = Some { isValid = true ; message = "" } }

let validateRequired = function 
    | { value = "" } as control -> { control with validation = Some { isValid = false ; message = "This field is required" } }
    | control -> { control with validation = Some { isValid = true ; message = "" } }

let isUrlPart input = System.Text.RegularExpressions.Regex.IsMatch(input, "^\w[\w\-]*$")

let validateUrlPart = function
    | { value = inputValue } as control when inputValue |> (isUrlPart >> not) -> { control with validation = Some { isValid = false ; message = "Only a-z, 0-9 and - (dash) are allowed" } } 
    | control -> { control with validation = Some { isValid = true ; message = "" } }

let isAbsoluteUrl input = System.Text.RegularExpressions.Regex.IsMatch(input, "^[a-z]+://")

let validateAbsoluteUrl minimumLength = function
    // don't validate unless over the minimum length
    // we do not want to have red warnings as the user just started writing
    | { value = inputValue } as control when (String.length inputValue) < minimumLength -> { control with validation = None }
    | { value = inputValue } as control when inputValue |> (isAbsoluteUrl >> not) -> { control with validation = Some { isValid = false ; message = "Only absolute URL's allowed, try to start with https:// or other protocol." } } 
    | control -> { control with validation = Some { isValid = true ; message = "" } }

let defaultFormControl =
    { value = ""
    ; validation = None
    }

type Form =
    { id : int Option
    ; originalUrl : FormControl
    ; shortUrl : FormControl
    }

type UrlRecord =
    { originalUrl : string
    ; shortUrl : string
    ; count : int
    }

type NotificationStatus = | Ok | Error | Info

type Notification =
    { message : string
    ; status : NotificationStatus
    }

type ViewModel =
    { notification : Notification option
    ; form : Form
    ; table : Map<int, UrlRecord>
    }