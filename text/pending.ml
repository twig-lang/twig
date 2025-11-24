open Frontend

let paths : string Stack.t = Stack.create ()
let processed : Env.variable Tree.t Map.t ref = ref Map.empty
let add path = Stack.push path paths
