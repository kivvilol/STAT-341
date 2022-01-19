library(regclass)

data(EX6.DONOR)
head(EX6.DONOR)

str(EX6.DONOR)

logit_md <- glm(Donate ~ TotalDonations, data = EX6.DONOR, family = 'binomial')
summary(logit_md)
visualize_model(logit_md)

logit_md2 <- glm(Donate ~ Age, data = EX6.DONOR, family = 'binomial')
summary(logit_md2)
visualize_model(logit_md2)

donor_modified <- na.omit(EX6.DONOR[ , c(1,3,4,5,6,7,11,16,18) ] )


data(EX6.CLICK)

log_md <- glm(Click ~ BannerPosition + SiteID + SiteCategory + AppDomain + AppCategory +
                DeviceModel, data = EX6.CLICK, family = 'binomial')
summary(log_md)

drop1(log_md, test = 'Chisq')

confusion_matrix(log_md)


data(EX6.WINE)

wine_md <- glm(Quality ~ volatile.acidity + alcohol, data = EX6.WINE, family = 'binomial')
visualize_model(wine_md)

wine_md2 <- glm(Quality ~ volatile.acidity + alcohol + volatile.acidity:alcohol, 
                data = EX6.WINE, family = 'binomial')
summary(wine_md2)
visualize_model(wine_md2)

wine_md3 <- glm(Quality ~ ., data = EX6.WINE, family = 'binomial')
summary(wine_md3)
visualize_model(wine_md3)
check_regression(wine_md3)

confusion_matrix(wine_md3)




md3_naive <- glm(Donate ~ 1, data = donor_modified, family = 'binomial')
md3_full <- glm(Donate ~.^2, data = donor_modified, family = 'binomial')
S <- step(md3_full, scope = list(lower = md3_naive, upper = md3_full), direction = 'both')
summary(S)
head(donor_modified)


md3_naive <- glm(Donate ~ 1, data = donor_modified, family = 'binomial')
md3_full <- glm(Donate ~.^2, data = donor_modified, family = 'binomial')
S <- step(md3_full, scope = list(lower = md3_naive, upper = md3_full), direction = 'forward')
summary(S)
head(donor_modified)
