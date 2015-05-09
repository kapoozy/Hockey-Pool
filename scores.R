getPoints <- function(player) {
    ## Scrape score table from nhl website
    url <- player[,4]
    doc <- htmlParse(url)
    tableNodes = getNodeSet(doc, "//table")
    tab = readHTMLTable(tableNodes[[3]])
    
    ## Get total playoff points for the year (thus far)
    points <- data.frame(lapply(tab[3,], as.character), stringsAsFactors=FALSE)[,5]
    return(as.numeric(points))
}

hockeyPool <- function(path) {
    ## Required libraries
    library(XML)
    
    ## Read player and team info
    teams <- read.csv(paste(path, "teams.csv", sep=""), stringsAsFactors=FALSE)
    player_list <- read.csv(paste(path, "players.csv", sep=""), stringsAsFactors=FALSE)
    
    ## Create data frames for each team
    team_scores <- data.frame("Team"=names(teams), "Points"=0)
    
    ## Get scores for each player on each team
    for(i in 1:ncol(teams)) {
        scores <- data.frame("Player"=1:nrow(teams[i]), "Points"=1:nrow(teams[i]))
        
        for(j in 1:nrow(teams[i])) {
            index <- which(player_list[,1] == teams[j,i])
            player <- player_list[index,]
            points <- getPoints(player)
            
            scores[j, 1] <- player[,2]
            scores[j, 2] <- points
        }
        
        team_scores[i, 2] <- sum(scores[,2])
        
        # print(names(teams[i]))
        # print(scores)
        # cat("\n")
    }
    
    ## Print leaderboard
    team_scores <- team_scores[order(team_scores$Points, decreasing=TRUE), ]
    # print(team_scores)
    outfile <- paste(path, "scores_", Sys.Date(), ".txt", sep="")
    write.table(team_scores, outfile, row.names=FALSE, col.names=FALSE)   
}