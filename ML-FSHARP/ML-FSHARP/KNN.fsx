#r @"M:\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.data\3.0.0\lib\net45\FSharp.Data.dll"
#r @"M:\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.charting\2.1.0\lib\net45\FSharp.Charting.dll"
#r @"M:\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.charting\2.1.0\lib\net45\FSharp.Charting.dll"
open FSharp.Data
open FSharp.Charting

//Importing Data
type importData = CsvProvider<"M:\ML-FSHARP\ML-FSHARP\DataR2.csv">
let data = importData.Load("M:\ML-FSHARP\ML-FSHARP\DataR2.csv")


//Age cancer true 
let ageTrue = [for r in data.Rows -> (r.Age,r.Classification.Equals(2))] 
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)

//Age Gluecose true 
let glucoseTrue = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(2))] 
                                            |> List.filter(fun (x,y) -> y = true) 
                                            |> List.map (fun (x,y)->x)

// Age cancer false
let ageFalse = [for r in data.Rows -> (r.Age,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)
//Glucose cancer false
let glucoseFalse = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)

//Combining attributes
let falseMix = List.map2 (fun x y -> (x,y)) ageFalse glucoseFalse 
let trueMix = List.map2 (fun x y -> (x,y)) ageTrue glucoseTrue

//Chart of Cancer comparison

Chart.Combine(
    [falseMix|>Chart.Point
     trueMix|>Chart.Point]) |> Chart.Show


//K Nearest Neighbor 
let mutable k1 = (0,0)
let mutable k2 = (0,0)
let mutable k3 = (0,0)

// Initialize Distance very high so it gets set
let mutable d1 = 10000.0 
let mutable d2 = 10000.0 
let mutable d3 = 10000.0

//raw data collection
let rawData = [for r in data.Rows -> (r.Age,r.Glucose)] //|>Chart.Point|>Chart.Show


let prediction (age:int) (glucose:int) (data:(int*int) List) = 
    data |> List.iter (fun (x,y)-> 
                                 //euclidean distance
                                 let dX:float = (float x) - (float age)
                                 let dY:float = (float y) - (float glucose)
                                 let distance:float = sqrt ((dX * dX) + (dY * dY))

                                 if (distance<d1) then 
                                    d1 <- distance
                                    k1 <- (x,y)
                                 else if (distance<d2) then 
                                    d2 <- distance
                                    k2 <- (x,y)
                                 else if (distance<d3) then 
                                    d3 <- distance
                                    k3 <- (x,y)
                                 )
    

prediction 57 126 rawData 

printf "1st: %A\n2nd: %A\n3rd: %A" k1 k2 k3

let kValues = [k1;k2;k3]
let input = [(57,126)]

Chart.Combine([rawData|>Chart.Point
               kValues|>Chart.Point
               input|>Chart.Point])|> Chart.Show

