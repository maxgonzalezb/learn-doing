
# LM Outputs
## These are two tables for each type of experience computation
## First computation of experience
regression.output.1=capture.output({stargazer(lm.4,lm.10, lm.25  ,lm.5, lm.11,lm.26, type = "latex",label = 'tab:table_exp_1', header = F,
                                              se = list(NULL, c(robust.lm4,robust.lm10,robust.lm25,robust.lm5,robust.lm11,robust.lm26)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications with Experience computed in rolling 2-year periods",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})

createStargazerTxt(regression.output.1,'table_ols_exp1.txt')


## Second computation of experience
regression.output.2=capture.output({stargazer(lm.16,lm.22, lm.27 , lm.17, lm.23,lm.28, type = "latex",label = 'tab:table_exp_2', header = F,
                                              se = list(NULL, c(robust.lm16,robust.lm22,robust.lm27,robust.lm17,robust.lm23,robust.lm28)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications with Experience computed as annualized cumulative experience",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})

createStargazerTxt(regression.output.2,'table_ols_exp2.txt')

