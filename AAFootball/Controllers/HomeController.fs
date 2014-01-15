namespace AAFootball.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax

// TODO: server both german and english pages


module Game = 
    
    type Team = A | B

    let randomNumberGenerator = System.Random()

    type Game() =

        let dicen(n) = randomNumberGenerator.Next(n)

        /// Gives a number between 1 to 6
        let dice6() = randomNumberGenerator.Next(6) + 1
        let dice10() = randomNumberGenerator.Next(10) + 1

        let switchSides (team:Team) = 
            match team with 
            | A -> B
            | B -> A
        let league = 
            [| "AC Milan"
               "Leverkuesen"
               "AnniSock"
               "Fulgart"
               "Liverpool"
               "Cambridge United" |]

(*
            [| "AnniSock"
               "Bayern Muenchen"
               "VfB Stuttgart"
               "Fulham"
               "Liverpool"
               "Cambridge United" |]
*)
        let teamA = dicen(league.Length)
        let teamB = (teamA + dicen(league.Length - 1) + 1) % league.Length


        let teamName (team:Team) = 
            match team with 
            | A -> league.[teamA]
            | B -> league.[teamB]

        let scoreGoal sideWithBall (goalsForA, goalsForB) =
             match sideWithBall with
             | A -> (goalsForA+1, goalsForB)
             | B -> (goalsForA, goalsForB+1)

        let maxScore (scoreForA, scoreForB) = max scoreForA scoreForB
        let minScore (scoreForA, scoreForB) = min scoreForA scoreForB
        
        let leadingSide (scoreForA, scoreForB) = 
            if scoreForA > scoreForB then A else B

        let totalRolls = 357
        let scoreText score = 
            let (scoreForA, scoreForB) = score
            if scoreForA = scoreForB then 
                sprintf "%d-%d" scoreForA scoreForB
            else 
                sprintf "%d-%d to %s" (maxScore score) (minScore score) (teamName (leadingSide score))

        let sideToKickOff = A
        let rec soccerGameKickOff () = 
          seq { 
              yield (sprintf "Today is the big match between %s and %s!" (teamName A) (teamName B))
              yield (sprintf "Kickoff by %s!!!" (teamName sideToKickOff))
              yield! soccerGame ((0,0),0,sideToKickOff)
          }
        and soccerGame (score, numRolls, sideWithBall) = 
          seq { 
              if numRolls = totalRolls then 
                  yield "Full time!!!!"
                  yield sprintf "The final score is %s!!!!!" (scoreText score)
              elif numRolls = totalRolls / 2 then 
                  yield "------"
                  yield "Half time!!!!"
                  yield "------"
                  yield sprintf "Restart after the half-time break by %s!!!" (teamName (switchSides sideToKickOff))
                  yield! soccerGame (score, numRolls+1, (switchSides sideToKickOff)) 
              else
                  let d  = dice10()
                  let sideName = teamName sideWithBall
                  let otherSide = switchSides sideWithBall
                  let otherSideName = teamName otherSide
                  let min = numRolls * 90 / totalRolls 
                  let sec = ((numRolls * 90) % totalRolls * 60) / totalRolls
                  let time = sprintf "%d:%02d" min sec
                  match d with 
                  | 1 | 2 -> 
                      yield sprintf "[%s] Great tackle by %s!!!" time otherSideName
                      yield! soccerGame (score, numRolls+1, otherSide) 
                  | 3 | 4 | 5 -> 
                      yield sprintf "[%s] Nice pass by %s!!!" time sideName
                      yield! soccerGame (score, numRolls+1, sideWithBall) 
                  | 6 -> 
                      yield sprintf "[%s] Shot by %s!!!" time sideName
                      match dice6() with 
                      | 1 | 2 -> 
                          yield sprintf "[%s] Great save by %s!!" time otherSideName
                          yield! soccerGame (score, numRolls+1, otherSide)  
                      | 3 | 4 | 5 -> 
                          yield sprintf "[%s] Nice save by %s!!" time otherSideName
                          yield sprintf "[%s] %s have the ball!!" time sideName
                          yield! soccerGame (score, numRolls+1, sideWithBall)  
                      | 6 | _ -> 
                          yield sprintf "[%s] GOAL to %s!!!!!!" time sideName
                          let newScore = scoreGoal sideWithBall score
                          yield sprintf "[%s] The score is now %s!!!!!!" time (scoreText newScore)
                          yield sprintf "[%s] Kickoff by %s!!!" time otherSideName
                          yield! soccerGame (newScore, numRolls+1, switchSides sideWithBall)  
                  | 7 | 8 | 9 | 10 -> 
                      yield! soccerGame (score, numRolls+1, sideWithBall) 
                  | _ -> failwith "too big" }
        member x.GetResults() = soccerGameKickOff ()

[<HandleError>]
type HomeController() =
    inherit Controller()
    member this.Index () =
        let gameResults = Game.Game().GetResults()
        let model = 
             seq { yield "Hello, welcome to Anna and Andreas' AA Football Website!"
                   yield! gameResults }
             
        this.View(model) :> ActionResult


