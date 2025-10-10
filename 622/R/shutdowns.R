# Shutdowns and Kaplan - Meier estimator.
t0 = 10;   # Current day of the shutdown

library(survival)

S = read.csv("C:\\Users\\baron\\Documents\\Teach\\622 Advanced Biostatistics\\R\\shutdowns.csv")
attach(S)
Duration[11] = t0;   # Updating duration of the current shutdown

# Kaplan-Meier ( ~ 1 means no grouping )
plot(survfit( Surv(Duration,Status) ~ 1 ), xlab="Days",ylab="Survival function")

# Conditioned on X > t, that the time to event has already exceeded t days
fit = survfit( Surv(Duration,Status) ~ 1 );
survival.function = fit$surv;
times = fit$time;

plot(times,survival.function,type="s");   # step
plot(times,survival.function,type="l");   # linear interpolation

t0 = 9;

cond_times = times[times > t0]
cond_surv.fn = survival.function[times > t0]/max(survival.function[times > t0])

lines(cond_times,cond_surv.fn,type="l",col="red");


# Cox proportional hazards model
S$SenatePropD = SenateD/(SenateD+SenateR)
S$HousePropD = HouseD/(HouseD+HouseR)

CoxFit = coxph( Surv(Duration,Status) ~ President + SenatePropD + HousePropD 
  + MonthStart + YearStart + DaysUntilCongressElect + 
  + DaysUntilPresidentElect + FYdeficitBln, data=S )

CoxFit = coxph( Surv(Duration,Status) ~ SenatePropD + HousePropD 
  + YearStart + DaysUntilCongressElect + 
  + DaysUntilPresidentElect, data=S )

h0 = survfit(CoxFit)         # Estimates the baseline survival S0(t) at X=0

newdata = S[11,];
pred_surv = survfit(CoxFit, newdata=newdata);      # Predicted survival curve
plot(pred_surv)  										# plots the survival curve
# pred_surv$time                                  # time points
# pred_surv$surv   # estimated survival probabilities at those times

# data.frame(pred_surv$time,pred_surv$surv)

# Adjust for survival up to time t0 (e.g., 6 days)

# Find S(t0 | X)
St0 = summary(pred_surv, times=t0)$surv

# Conditional survival: S(t | T > t0)
cond_surv = pred_surv$surv / St0
cond_surv[pred_surv$time < t0] <- NA  # optional: only show t >= t0
pred_surv$cond_surv = cond_surv;

plot(pred_surv$time, cond_surv, type="s", xlab="Time", ylab="Conditional Survival")

Day = pred_surv$time; CondSurvProb = cond_surv;
data.frame(Day, CondSurvProb)[Day >= t0,]


### Include the budget deficit

CoxFit = coxph( Surv(Duration,Status) ~ SenatePropD + HousePropD 
  + YearStart + DaysUntilCongressElect + 
  + DaysUntilPresidentElect + FYdeficitBln, data=S )

h0 = survfit(CoxFit)         # Estimates the baseline survival S0(t) at X=0

newdata = S[11,];
pred_surv = survfit(CoxFit, newdata=newdata);      # Predicted survival curve

# Adjust for survival up to time t0 (e.g., 6 days)
# Find S(t0 | X)
St0 = summary(pred_surv, times=t0)$surv

# Conditional survival: S(t | T > t0)
cond_surv = pred_surv$surv / St0
cond_surv[pred_surv$time < t0] <- NA  # optional: only show t >= t0
pred_surv$cond_surv = cond_surv;

plot(pred_surv$time, cond_surv, type="l", xlab="Time", ylab="Conditional Survival")

Day = pred_surv$time; CondSurvProb = cond_surv;
data.frame(Day, CondSurvProb)[Day >= t0,]



