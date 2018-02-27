food   <- fread('food.csv')
lifts  <- fread('lifts.csv')
weight <- fread('weight.csv')

getWeightByFoodRegimeG <- function(weight, food) {
    subWeight <- filter(weight, date >= min(food$startDate), date <= max(food$endDate))
    regimeIdx <- vapply(subWeight$date, function(x) which(food$startDate <= x & food$endDate >= x), numeric(1))
    subWeight$regime <- as.factor(food$calories[regimeIdx])

    g <- ggplot(subWeight, aes(y=bw, x=ymd(date), color=regime)) + geom_step()

    invisible(g)
}

getVolumeForExerciseG <- function(lifts, exercises) {
    subLifts <- filter(lifts, exercise %in% exercises) %>% as.data.table()
    if (nrow(subLifts) < 0) stop(sprintf('No entries for exercise %s', ex))

    volumeDF <- subLifts[,.(volume=sum(sets*reps*weight)), by=date]

    g <- ggplot(volumeDF, aes(y=volume, x=ymd(date))) + geom_step() + geom_smooth()

    res <- list()
    res$g        <- g
    res$volumeDF <- volumeDF

    invisible(res)
}
