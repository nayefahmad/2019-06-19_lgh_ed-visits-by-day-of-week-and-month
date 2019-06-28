# LGH daily and hourly ED visits models 
The goal here is not to produce a highly accurate *predictive* algorithm, but rather to: 

* Find how much of the variance in daily ED visits can be explained with weekday, year, month, and lagged values as predictors. If, for example, only 30% of variance is explained, then it is misleading to display only the averages by day of week in a graph. This would give the impression of precision where it is not justified. 
* Fest for interactions between weekday and month variables. If there is no interaction, there is no point in "slicing" data in this way to try to find e.g. whether Mondays in February are different from Mondays in September. There just isn't enough data to support these inferences. 