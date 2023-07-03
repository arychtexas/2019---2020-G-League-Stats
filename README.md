# 2019---2020-G-League-Stats

1. Welcome to the NBA Gatorade League (G-League) Dashboard. This Dashboard was built using data from 2019-2020. This dashboard contains a search function, charts, filters, and two new statistical categories I created: Impact Player Score and Star Rating.

1. IPS --Impact Player Score. Impact Player Score metric is based on John Hollinger's statistic  Gane Score: (GmSc) - Game Score; the formula: 

> PTS + 0.4 * FG - 0.7 * FGA - 0.4*(FTA - FT) + 0.7 * ORB + 0.3 * DRB +  STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV. 

3. The IPS divides the previous formula by the minutes per game and this score should give us outliers, players who excelled beyond the average player and those who perform poorly while on the court. This statistic isn't as complex as P.E.R. (for we do not have the team data to calculate P.E.R.) but can weed out less productive players quickly. 

Impact Player Score Formula : 

> IPS = (PTS + 0.4 * FG - 0.7 * FGA - 0.4 * (FTA - FT) + 0.7 * ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV) / (MP / G)

4. StarRating --Star Rating is a 1-5 grading system based on a player's IPS in relation to the entire G League. This rating is divided into quantiles. The distribution is as follows:

  * Star Rating 5 = is top 10% of IPS scores
  * Star Rating 4 = is top 11-30% of IPS scores 
  * Star Rating 3 = is top 31-60% of IPS scores
  * Star Rating 2 = is top 61-90% of IPS scores
  * Star Rating 1 = is top 91-100% of IPS scores

5. The NBA Gatorade League (G-League), formerly the NBA Development League or NBA D-League, is the NBA's official minor league, preparing players, coaches, officials, trainers, and front office staff for the NBA while acting as the league's research and development laboratory. Beginning with the 2017-18 season, the NBA D-League became the NBA G League as part of a multiyear expanded partnership between the NBA and Gatorade. Each team plays a 50-game schedule (18 showcases, 32 regular seasons).

6. To start the 2022-23 season, a record 47 percent of players on opening-night NBA rosters had NBA G League experience. Forty-one percent of players on NBA start-of-season rosters for 2021-22 had NBA G League experience. This group includes players who were assigned from the NBA to the NBA G League and were called up from the NBA G League to the NBA at some point in their careers. At least 30 NBA G League prospects have been called up to the NBA in the past eight seasons. A record 132 players were assigned to the NBA G League 579 times in 2021-22, while a record 164 Gatorade Call-Ups of 117 players occurred during the 2021-22 season. Who are some of the best NBA players who have played in the NBA G League? Top players who have been assigned to the NBA G League include guard Eric Bledsoe, center Rudy Gobert, guard Reggie Jackson, and center Clint Capela.

7. DISCLAIMER: This data and its analysis are provided for informational purposes only. The information presented here is not endorsed, affiliated with, or sponsored by the NBA G League or any related entities. The data used in this analysis is publicly available and has been collected from various sources. We make no representations or warranties of any kind, express or implied, about the data's completeness, accuracy, reliability, or suitability. Any reliance you place on the information provided is strictly at your own risk. We will not be liable for any loss or damage arising from using this data. The use of this data does not create a professional-client relationship. We recommend verifying the data with official sources before making decisions or conclusions.
