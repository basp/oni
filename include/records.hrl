-record(object, {id, 
                 parent,
                 name = "", 
                 location = nothing, 
                 owner = nothing, 
                 properties = [],
                 flags = 2#000000}).

-record(property, {name, 
                   value}).