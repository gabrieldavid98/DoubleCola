type Group = { GroupSize: int; GroupName: string }

type SeqDestruct<'a> =
    | EmptySeq
    | Cons of head : 'a * tail : seq<'a>

let doubleGroup (group: Group) =
    { group with GroupSize = group.GroupSize * 2 }

let seqDestruct<'a> (seq : seq<'a>) =
    match seq with
    | s when Seq.isEmpty s -> EmptySeq
    | s -> Cons(Seq.head s, Seq.tail s)

let names = [ "Sheldon"; "Leonard"; "Penny"; "Rajesh"; "Howard" ]

let rec groups names =
    seq { yield! (Seq.map (fun name -> { GroupSize = 1; GroupName = name }) names)
          yield! (Seq.map doubleGroup (groups names)) }

let rec nth n (groups : seq<Group>) =
    match seqDestruct groups with
    | EmptySeq -> failwith "unreachable"
    | Cons(head, tail) -> if n <= head.GroupSize then head.GroupName else nth (n - head.GroupSize) tail

printfn "%s" (nth 10010 (groups names)) // >> Howard