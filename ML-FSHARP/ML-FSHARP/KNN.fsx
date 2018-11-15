#r @"C:\Users\Mikes\Desktop\KNN\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.data\3.0.0\lib\net45\FSharp.Data.dll"
#r @"C:\Users\Mikes\Desktop\KNN\ML-FSHARP\ML-FSHARP\.nuget\packages\fsharp.charting\2.1.0\lib\net45\FSharp.Charting.dll"

open FSharp.Data
open FSharp.Charting

// Importing Data
type importData = CsvProvider<"C:\Users\Mikes\Desktop\KNN\ML-FSHARP\ML-FSHARP\DataR2.csv">
let data = importData.Load("C:\Users\Mikes\Desktop\KNN\ML-FSHARP\ML-FSHARP\DataR2.csv")


// Age Cancer True 
let ageTrue = [for r in data.Rows -> (r.Age,r.Classification.Equals(2))] 
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Age Cancer False
let ageFalse = [for r in data.Rows -> (r.Age,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)


(* ====== Visualization of Age and Glucose Cancer ====== *)
// Gluecose Cancer True 
let glucoseTrue = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(2))] 
                                            |> List.filter(fun (x,y) -> y = true) 
                                            |> List.map (fun (x,y)->x)

// Glucose Cancer False
let glucoseFalse = [for r in data.Rows -> (r.Glucose,r.Classification.Equals(1))] 
                                            |> List.filter(fun(x,y) -> y = true)
                                            |> List.map (fun (x,y)->x)
// Combining (AGE,GLUCOSE) attributes
let falseAgeGlucose = List.map2 (fun x y -> (x,y)) ageFalse glucoseFalse 
let trueAgeGlucose = List.map2 (fun x y -> (x,y)) ageTrue glucoseTrue

(*Cancer Comparison (AGE,GLUCOSE)*) 
//Chart.Combine([
//               falseAgeGlucose|>Chart.Point
//              trueAgeGlucose|>Chart.Point]) |> Chart.Show
 

(* ====== Visualization of Age and BMI Cancer ====== *)
// BMI Cancer true 
let bmiTrue = [for r in data.Rows -> (r.BMI,r.Classification.Equals(2))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// BMI Cancer false 
let bmiFalse = [for r in data.Rows -> (r.BMI,r.Classification.Equals(1))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Combining (AGE,BMI) attributes
let falseAgeBMI = List.map2 (fun x y -> (x,y)) ageFalse bmiFalse 
let trueAgeBMI = List.map2 (fun x y -> (x,y)) ageTrue bmiTrue

(*Cancer Comparison (AGE,BMI)*)
//Chart.Combine([
//               falseAgeBMI|>Chart.Point
//               trueAgeBMI|>Chart.Point])|> Chart.Show


(* ====== Visualization of Age and Insulin Cancer ====== *)
// Insulin Cancer true 
let insulinTrue = [for r in data.Rows -> (r.Insulin,r.Classification.Equals(2))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Insulin Cancer false 
let insulinFalse = [for r in data.Rows -> (r.Insulin,r.Classification.Equals(1))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Combining (AGE,Insulin) attributes
let falseAgeInsulin = List.map2 (fun x y -> (x,y)) ageFalse insulinFalse 
let trueAgeInsulin = List.map2 (fun x y -> (x,y)) ageTrue insulinTrue

(*Cancer Comparison (AGE,Insulin)*)
//Chart.Combine([
//               falseAgeInsulin|>Chart.Point
//               trueAgeInsulin|>Chart.Point])|> Chart.Show


(* ====== Visualization of Risistin and Insulin Cancer ====== *)
// Resistin Cancer true 
let risistinTrue = [for r in data.Rows -> (r.Resistin,r.Classification.Equals(2))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Risistin Cancer false 
let risistinFalse = [for r in data.Rows -> (r.Resistin,r.Classification.Equals(1))]
                                        |> List.filter(fun (x,y) -> y = true) 
                                        |> List.map (fun (x,y)->x)
// Combining (Insulin,Risistin) attributes
let falseInsulinRisistin = List.map2 (fun x y -> (x,y)) insulinFalse risistinFalse 
let trueInsulinRisistin = List.map2 (fun x y -> (x,y)) insulinTrue risistinTrue

(*Cancer Comparison (Insulin,Risistin)*)
//Chart.Combine([
//                falseInsulinRisistin|>Chart.Point
//                trueInsulinRisistin|>Chart.Point])|> Chart.Show
               

// K Nearest Neighbor

// (INT,INT)
let mutable intIntK1 = (0,0)
let mutable intIntK2 = (0,0)
let mutable intIntK3 = (0,0)

// (INT,DECIMAL)
let mutable intDecimalK1 = (0,0.0m)
let mutable intDecimalK2 = (0,0.0m)
let mutable intDecimalK3 = (0,0.0m)

// (DECIMAL,INT)
let mutable decimalIntK1 = (0.0m,0)
let mutable decimalIntK2 = (0.0m,0)
let mutable decimalIntK3 = (0.0m,0)

// (DECIMAL,DECIMAL)
let mutable decimalDecimalK1 = (0.0m,0.0m)
let mutable decimalDecimalK2 = (0.0m,0.0m)
let mutable decimalDecimalK3 = (0.0m,0.0m)
let mutable decimalDecimalK4 = (0.0m,0.0m)
let mutable decimalDecimalK5 = (0.0m,0.0m)

// Initialize Distance very high so it gets set
let mutable d1 = 9999999.99 
let mutable d2 = 9999999.99 
let mutable d3 = 9999999.99
//CURRENTLY ONLY IN DECIMALDECIMAL FUNCTION
let mutable d4 = 9999999.99
let mutable d5 = 9999999.99

// Cancer Classification 
let mutable c1 = 0 
let mutable c2 = 0 
let mutable c3 = 0
//CURRENTLY ONLY IN DECIMALDECIMAL FUNCTION
let mutable c4 = 0
let mutable c5 = 0


(*==== (INT,INT) Prediction for KNN ====*) 
let predictionIntInt inputX inputY data = 
    
    // Reset distance very high 
    d1 <- 9999999.99 
    d2 <- 9999999.99 
    d3 <- 9999999.99

    data |> List.iter (fun (x,y,z)-> 
                                 //Euclidean Distance Calculation
                                 let dX:float = (float x) - (float inputX)
                                 let dY:float = (float y) - (float inputY)
                                 let distance:float = sqrt ((dX * dX) + (dY * dY))

                                 if (distance<d1) then 
                                    //Set new closet distance 
                                    d1 <- distance
                                    //Set Cancer chance
                                    c1 <- z
                                    //Set Closet K
                                    intIntK1 <- (x,y)
                                 
                                 else if (distance<d2) then 
                                    //Set new closet distance 
                                    d2 <- distance
                                    //Set Cancer chance
                                    c2 <- z
                                    //Set 2nd Closet K
                                    intIntK2 <- (x,y)
                                   
                                 else if (distance<d3) then 
                                    //Set new closet distance
                                    d3 <- distance
                                    //Set Cancer chance
                                    c3 <- z
                                    //Set 3rd Closet K
                                    intIntK3 <- (x,y)
                                 )


(*==== (INT,DECIMAL) Prediction for KNN ====*) 
let predictionIntDecimal inputX (inputY:decimal) (data:(int*decimal*int) List) = 

    // Reset distance very high 
    d1 <- 9999999.99 
    d2 <- 9999999.99 
    d3 <- 9999999.99

    data |> List.iter (fun (x,y,z)-> 
                                 //Euclidean Distance Calculation
                                 let dX:float = (float x) - (float inputX)
                                 let dY:float = (float y) - (float inputY)
                                 let distance:float = sqrt ((dX * dX) + (dY * dY))

                                 if (distance<d1) then 
                                    //Set new closet distance 
                                    d1 <- distance
                                    //Set Cancer chance
                                    c1 <- z
                                    //Set Closet K
                                    intDecimalK1 <- (x,y)
                                 
                                 else if (distance<d2) then 
                                    //Set new closet distance 
                                    d2 <- distance
                                    //Set Cancer chance
                                    c2 <- z
                                    //Set 2nd Closet K
                                    intDecimalK2 <- (x,y)
                                   
                                 else if (distance<d3) then 
                                    //Set new closet distance
                                    d3 <- distance
                                    //Set Cancer chance
                                    c3 <- z
                                    //Set 3rd Closet K
                                    intDecimalK3 <- (x,y)
                                 )

(*==== (DECIMAL,DECIMAL) Prediction for KNN ====*) 
let predictionDecimalDecimal (inputX:decimal) (inputY:decimal) (data:(decimal*decimal*int) List) = 

    // Reset distance very high 
    d1 <- 9999999.99 
    d2 <- 9999999.99 
    d3 <- 9999999.99
    d4 <- 9999999.99
    d5 <- 9999999.99

    data |> List.iter (fun (x,y,z)-> 
                                 //Euclidean Distance Calculation
                                 let dX:float = (float x) - (float inputX)
                                 let dY:float = (float y) - (float inputY)
                                 let distance:float = sqrt ((dX * dX) + (dY * dY))

                                 if (distance<d1) then 
                                    //Set new closet distance 
                                    d1 <- distance
                                    //Set Cancer chance
                                    c1 <- z
                                    //Set Closet K
                                    decimalDecimalK1 <- (x,y)
                                 
                                 else if (distance<d2) then 
                                    //Set new closet distance 
                                    d2 <- distance
                                    //Set Cancer chance
                                    c2 <- z
                                    //Set 2nd Closet K
                                    decimalDecimalK2 <- (x,y)
                                   
                                 else if (distance<d3) then 
                                    //Set new closet distance
                                    d3 <- distance
                                    //Set Cancer chance
                                    c3 <- z
                                    //Set 3rd Closet K
                                    decimalDecimalK3 <- (x,y)

                                 else if (distance<d4) then 
                                    //Set new closet distance
                                    d4 <- distance
                                    //Set Cancer chance
                                    c4 <- z
                                    //Set 4th Closet K
                                    decimalDecimalK4 <- (x,y)

                                 else if (distance<d5) then 
                                    //Set new closet distance
                                    d5 <- distance
                                    //Set Cancer chance
                                    c5 <- z
                                    //Set 5th Closet K
                                    decimalDecimalK5 <- (x,y)
                                 )

(*

(*====== !!!GLUCOSE HAS BEEN DEVIDED BY 3!!! ======*)
(*       *In Example shown below 
         *This has been done to 
         *Normalize the data*)

// normalized data (Age,Glucose) collection
let dataAgeGlucose = [for r in data.Rows -> (r.Age,(fun x -> x/3)r.Glucose,r.Classification)] 
// KNN (Age,Glucose)
predictionIntInt 83 (170/3) dataAgeGlucose 
(*====Visualization====*)
let kValues = [intIntK1;intIntK2;intIntK3]
let input = [(83,170/3)]

//Normalizse Glocose
let normGlucoseTrue = [for r in glucoseTrue -> (fun x-> x/3)r]
let normGlucoseFalse = [for r in glucoseFalse -> (fun x-> x/3)r]
//Map (Age,Glocose) **Normalized Glucose**
let normAgeGlucoseFalse = List.map2 (fun x y -> (x,y)) ageFalse normGlucoseFalse
let normAgeGlucoseTrue = List.map2 (fun x y -> (x,y)) ageTrue normGlucoseTrue

//Showing Graphs
Chart.Combine([normAgeGlucoseFalse|>Chart.Point
               normAgeGlucoseTrue|>Chart.Point
               kValues|>Chart.Point
               input|>Chart.Point])|> Chart.Show



(*====== !!!BMI HAS BEEN MULTIPLED BY 2.5!!! ======*)
(*       *In Example shown below 
         *This has been done to 
         *Normalize the data*)

let dataAgeBMI = [for r in data.Rows -> (r.Age,(fun x -> x*2.5m)r.BMI,r.Classification)] 
// KNN (Age,BMI)
predictionIntDecimal 43 (30.0m*2.5m) dataAgeBMI 
(*====Visualization ====*)
let kValues2 = [intDecimalK1;intDecimalK2;intDecimalK3]
let input2 = [(43,(30.0m*2.5m))]

//Normalizse BMI
let normBMITrue = [for r in bmiTrue -> (fun x-> x*2.5m)r]
let normBMIFalse = [for r in bmiFalse -> (fun x-> x*2.5m)r]
//Map (Age,BMI) **BMI Glucose**
let normAgeBMIFalse = List.map2 (fun x y -> (x,y)) ageFalse normBMIFalse 
let normAgeBMITrue = List.map2 (fun x y -> (x,y)) ageTrue normBMITrue

Chart.Combine([normAgeBMIFalse|>Chart.Point
               normAgeBMITrue|>Chart.Point
               kValues2|>Chart.Point
               input2|>Chart.Point])|> Chart.Show



(*====== !!!RAW AGE AND INSULIN!!! ======*)
(*       *In Example shown belowa*)

// raw data collection 
let dataAgeInsulin = [for r in data.Rows -> ((fun x -> x)r.Age,(fun x ->x)r.Insulin,r.Classification)]

predictionIntDecimal (48) (5m) dataAgeInsulin 

// Combining (AGE,Insulin) attributes
let ageInsulinFalse = List.map2 (fun x y -> (x,y)) ageFalse insulinFalse 
let ageInsulinTrue = List.map2 (fun x y -> (x,y)) ageTrue insulinTrue

let kValues3 = [intDecimalK1;intDecimalK2;intDecimalK3]
let input3 = [(48,5m)]

(*Cancer Comparison (AGE,Insulin)*)
Chart.Combine([
               ageInsulinFalse|>Chart.Point
               ageInsulinTrue|>Chart.Point
               kValues3|>Chart.Point
               input3|>Chart.Point])|> Chart.Show
*)

(*====== !!!RAW RESISTIN AND INSULIN!!! ======*)
(*       *In Example shown below*)

// raw data collection 
let dataRisistinInsulin = [for r in data.Rows -> ((fun x -> x)r.Resistin,(fun x ->x)r.Insulin,r.Classification)]

predictionDecimalDecimal (22m) (5m) dataRisistinInsulin 

// Combining (Risistin,Insulin) attributes
let risistinInsulinFalse = List.map2 (fun x y -> (x,y)) risistinFalse insulinFalse 
let risistinInsulinTrue = List.map2 (fun x y -> (x,y)) risistinTrue insulinTrue

let kValues4 = [decimalDecimalK1;decimalDecimalK2;decimalDecimalK3;decimalDecimalK4;decimalDecimalK5]
let input4 = [(22,5m)]

(*Cancer Comparison (AGE,Insulin)*)
Chart.Combine([
               risistinInsulinFalse|>Chart.Point
               risistinInsulinTrue|>Chart.Point
               kValues4|>Chart.Point
               input4|>Chart.Point])|> Chart.Show


let sum = c1+c2+c3


(*==== Complete at the end ====
let cancerSum = 
    let sum =c1+c2+c3
    match sum with
    | 3 -> "Cancer LOWEST Chance"
    | 4 -> "Cancer unlikely"
    | 5 -> "Cancer likely"
    | 6 -> "Cancer HIGHEST Chance"
    | _ -> "Error with cancerSum"
*)
