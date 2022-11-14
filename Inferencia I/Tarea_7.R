n<-1919
X<-922
hat_theta<-X/n
#hat_theta = 0.4804586
hat_se<-sqrt(hat_theta*(1-hat_theta)/n)
#hat_se = 0.01140514
est_wald<-abs((hat_theta-0.5)/hat_se)
#est_wald = 1.713388
alpha<-0.95# Confianza del 0.95
z_alpha_2<-abs(qnorm(1-alpha))
#z_alpha_2 = 1.644854
col_izq<-hat_theta-hat_se*z_alpha_2
col_der<-hat_theta+hat_se*z_alpha_2
#interva de confianza (0.4616988, 0.4992184)
p_valor<-2*(1-pnorm(abs(est_wald)))
#p-valor = 0.08664119