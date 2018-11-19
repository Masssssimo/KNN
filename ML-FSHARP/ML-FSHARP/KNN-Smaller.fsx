#r @"E:\UNI YR 3\Computer Applications\Project\KNN\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.data\3.0.0\lib\net45\FSharp.Data.dll"
#r @"E:\UNI YR 3\Computer Applications\Project\KNN\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.charting\2.1.0\lib\net45\FSharp.Charting.dll"

open FSharp.Data
open FSharp.Charting

// Importing Data
type importData = CsvProvider<"E:\UNI YR 3\Computer Applications\Project\KNN\ML-FSHARP\ML-FSHARP\DataR2.csv">
let data = importData.Load("E:\UNI YR 3\Computer Applications\Project\KNN\ML-FSHARP\ML-FSHARP\DataR2.csv")

(*K Nearest Neighbour*)

// K Values (INT,INT)
let mutable K1 = (0,0)
let mutable K2 = (0,0)
let mutable K3 = (0,0)

// Initialize Distance Very High
let mutable D1 = 9999999.99 
let mutable D2 = 9999999.99 
let mutable D3 = 9999999.99

// Cancer Classification 
let mutable C1 = 0 
let mutable C2 = 0 
let mutable C3 = 0



let KNN inputX inputY data = 
     data |> List.iter (fun (x,y,z)-> 
                                 //Euclidean Distance Calculation
                                 let dX:float = (float x) - (float inputX)
                                 let dY:float = (float y) - (float inputY)
                                 let distance:float = sqrt ((dX * dX) + (dY * dY))

                                 match distance with
                                 | distance when distance<D1 -> 
                                    //Reset K3
                                    K3 <- K2    
                                    D3 <- D2
                                    C3 <- C2
                                    //Reset K2
                                    K2 <- K1
                                    D2 <- D1
                                    C2 <- C1
                                    //Set New Closet Distance 
                                    D1 <- distance
                                    //Set Cancer Classification 
                                    C1 <- z
                                    //Set Closet K
                                    K1 <- (x,y)
                                 | distance when (distance<D2)||(distance=D1) ->
                                    //Reset K3
                                    K3 <- K2
                                    D3 <- D2
                                    C3 <- C2
                                    //Set 2nd Closet Distance 
                                    D2 <- distance
                                    //Set Cancer Classification 
                                    C2 <- z
                                    //Set 2nd Closet K Value
                                    K2 <- (x,y)
                                 | distance when (distance<D3)||(distance=D2) ->
                                    //Set 3rd Closet distance
                                    D3 <- distance
                                    //Set Cancer chance
                                    C3 <- z
                                    //Set 3rd Closet K
                                    K3 <- (x,y)
                                 |__-> ()                               
                                 )

// Age Cancer True 
let ageTrue = [for r in data.Rows -> (r.Age,r.Classification.Equals(2))] 
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Age Cancer False
let ageFalse = [for r in data.Rows -> (r.Age,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)

// Gluecose Cancer True 
let glucoseTrue = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(2))] 
                                            |> List.filter(fun (x,y) -> y = true) 
                                            |> List.map (fun (x,y)->x)

// Glucose Cancer False
let glucoseFalse = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)
// Combining (AGE,GLUCOSE) attributes
let falseAgeGlucose = List.map2 (fun x y -> (x,y/3)) ageFalse glucoseFalse 
let trueAgeGlucose = List.map2 (fun x y -> (x,y/3)) ageTrue glucoseTrue

// Normalized Data (Age,Glucose/3) 
let dataAgeGlucose = [for r in data.Rows -> (r.Age,(fun x -> x/3)r.Glucose,r.Classification)]  



KNN (28) (85/3) dataAgeGlucose

let input = [(28,85/3)]
let kValues = [K1;K2;K3]

//Showing Graphs
Chart.Combine([falseAgeGlucose|>Chart.Point
               trueAgeGlucose|>Chart.Point
               kValues|>Chart.Point
               input|>Chart.Point])|> Chart.Show


               
let kTotal = C1 + C2 + C3
printf "K Value Total = %i\n" kTotal

               
