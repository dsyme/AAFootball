namespace AAFootball.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax

// TODO: server both german and english pages

type Model = 
    { Events: seq<string> 
      ImageA: string
      ImageB: string }

type Team = A | B

type FreebaseData = FSharp.Data.FreebaseDataProvider<Key="AIzaSyD57qujZe2H2bGJEupNEBJkj0Z83zefWcM">

type Game() =

    static let te x = (x :> FreebaseData.ServiceTypes.Soccer.Soccer.Football_teamData)

    let randomNumberGenerator = System.Random()
    let dicen(n) = randomNumberGenerator.Next(n)

    /// Gives a number between 1 to 6
    let dice6() = randomNumberGenerator.Next(6) + 1
    let dice10() = randomNumberGenerator.Next(10) + 1

    let switchSides (team:Team) = 
        match team with 
        | A -> B
        | B -> A

    static let league = 
        let conn = FreebaseData.GetDataContext()
(*
        let teams = 
            query { for team in conn.Sports.Soccer.``Football teams`` do 
                    take 100
                    select team }
            |> Seq.filter (fun team -> not (String.IsNullOrWhiteSpace(team.MainImage)))
            |> Seq.toArray

*)

        let teams = 
           [| 
                "Germany", te conn.Sports.Soccer.``Football teams``.Individuals10.``Germany national football team``, 
                   "http://upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/200px-Flag_of_Germany.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of England``.MainImage
                "England", te conn.Sports.Soccer.``Football teams``.Individuals10.``England national football team``, 
                   "http://upload.wikimedia.org/wikipedia/en/thumb/b/be/Flag_of_England.svg/200px-Flag_of_England.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of England``.MainImage
                "United States", te conn.Sports.Soccer.``Football teams``.Individuals10.``United States men's national soccer team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/e/e2/Flag_of_the_United_States_%28Pantone%29.svg/200px-Flag_of_the_United_States_%28Pantone%29.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of the United States``.MainImage
                "Australia", te conn.Sports.Soccer.``Football teams``.Individuals10.``Australia national association football team``, 
                   "http://upload.wikimedia.org/wikipedia/en/thumb/b/b9/Flag_of_Australia.svg/200px-Flag_of_Australia.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Australia``.MainImage
                "Spain", te conn.Sports.Soccer.``Football teams``.Individuals10.``Spain national football team``, 
                   "http://upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/200px-Flag_of_Spain.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Spain``.MainImage
                "Ghana", te conn.Sports.Soccer.``Football teams``.Individuals10.``Ghana national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Ghana.svg/200px-Flag_of_Ghana.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Ghana``.MainImage
                "Portugal", te conn.Sports.Soccer.``Football teams``.Individuals10.``Portugal national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Portugal.svg/200px-Flag_of_Portugal.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Portugal``.MainImage
                "Iran", te conn.Sports.Soccer.``Football teams``.Individuals10.``Iran national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/Flag_of_Iran.svg/200px-Flag_of_Iran.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Iran``.MainImage
                "Algeria", te conn.Sports.Soccer.``Football teams``.Individuals10.``Algeria national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_Algeria.svg/200px-Flag_of_Algeria.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Algeria``.MainImage
                "Netherlands", te conn.Sports.Soccer.``Football teams``.Individuals10.``Netherlands national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/200px-Flag_of_the_Netherlands.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of the Kingdom of the Netherlands``.MainImage
                "Honduras", te conn.Sports.Soccer.``Football teams``.Individuals10.``Honduras national football team``, 
                   "http://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Flag_of_Honduras.svg/200px-Flag_of_Honduras.svg.png"
                   //conn.Commons.Symbols.Flags.Individuals10.``Flag of Honduras``.MainImage
                
            |] 

        teams




(*
        [| "AC Milan"
            "Leverkuesen"
            "AnniSock"
            "Fulgart"
            "Liverpool"
            "Cambridge United" |]
*)
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

    let getTeam team = 
        match team with 
        | A -> league.[teamA]
        | B -> league.[teamB]

    let teamName team = let (n, _, _) = getTeam team in n 
    let teamImage team = let (_, _, url) = getTeam team in url

    let scoreGoal sideWithBall (goalsForA, goalsForB) =
            match sideWithBall with
            | A -> (goalsForA+1, goalsForB)
            | B -> (goalsForA, goalsForB+1)

    let maxScore (scoreForA, scoreForB) = max scoreForA scoreForB
    let minScore (scoreForA, scoreForB) = min scoreForA scoreForB
        
    let leadingSide (scoreForA, scoreForB) = 
        if scoreForA > scoreForB then A else B

    let totalRolls = 200
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

    member x.GetResults() = 
        { Events = soccerGameKickOff () 
          ImageA = teamImage A
          ImageB = teamImage B }

[<HandleError>]
type HomeController() =
    inherit Controller()
    member this.Index () =
        let model = Game().GetResults()
        this.View(model) :> ActionResult


