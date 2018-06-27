namespace Shared

type Counter = int

type Validation =
    { isValid : bool
    ; message : string
    }

type FormControl =
    { value : string
    ; validation : Validation Option
    }

type Form =
    { originalUrl : FormControl
    ; shortUrl : FormControl
    }

type UrlRecord =
    { id : int option
    ; originalUrl : string
    ; shortUrl : string
    ; count : int
    }

type ViewModel =
    { form : Form
    ; table : UrlRecord list
    }