if exists("b:current_syntax")
    finish
endif

set iskeyword+=-

syntax keyword lensKeyword physical section layergroup layer chordgroup block end
syntax keyword lensAction sticky-switch switch
syntax keyword lensFunction columns place action key after before
syntax keyword lensLayerName center topleft topright bottomleft bottomright

syntax match lensComment "\v--.*$"
syntax match lensOperator "\v\=\>"
syntax match lensHexColor "#[0-9a-fA-F]\{6\}"

highlight link lensAction Identifier
highlight link lensKeyword Keyword
highlight link lensLayerName Constant
highlight link lensComment Comment
highlight link lensOperator Operator
highlight link lensHexColor String
highlight link lensFunction Function

let b:current_syntax = "lens"
