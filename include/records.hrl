-record(object, {id, 
                 parent,
                 name = "", 
                 location = nothing, 
                 owner = nothing, 
                 properties = [],
                 flags = 2#00000}).

-record(property, {name, 
                   value}).