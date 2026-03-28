# -19-07-2023 
# Plot neutral model fitting plot based on the output of neutral_model.R 
# model.fit: matrix after neutral model fitting with fitted curve, confidence interval...
# model.stat: containing parameters of non-linear square fitting (m,R2,standard error for the regression)

# Plot options:
# Make the mixing plot with scatter points and fitted curve with upper and lower boundary
# geom_line (linetype=c["solid","dashed","longdash","dotted","dotdash","blank")
# mytheme <- theme_classic(base_line_size=0.8)  # half border
library(ggplot2)
library(dplyr)

nmf_plot <- function(model.fit,model.stat) {
        for (i in 1:nrow(model.fit)) {
                m <- model.fit[i,]
                if (m$freq>=m$pred.lwr & m$freq<=m$pred.upr) {
                        model.fit$tax_type[i] <- "Predicted"
                } else if (m$freq>m$pred.upr) {
                        model.fit$tax_type[i] <- "Above"
                } else {
                        model.fit$tax_type[i] <- "Below"
                }
        }
        model.fit$tax_type <- factor(model.fit$tax_type,levels=c("Above","Predicted","Below"))
        tax_color <- c("green","blue","orange")
        mytheme <- theme_bw() +
                   theme(panel.grid.major = element_blank(),                                       # Remove major grid
                         panel.grid.minor = element_blank(),
                         panel.border = element_rect(color="black",fill=NA,size=0.8),
                         panel.background = element_rect(fill="white"),
                         axis.text.x=element_text(color="black",size=12,angle=0,vjust=.5,hjust=.5),
                         axis.text.y=element_text(color="black",size=12),
			 axis.title.x=element_text(color="black",size=14,face="bold"),
			 axis.title.y=element_text(color="black",size=14,face="bold"))
        p <- ggplot(model.fit,aes(log10(p))) +
                geom_line(aes(y=freq.pred),color="black",linetype="solid") +
                geom_line(aes(y=pred.lwr),color="black",linetype="dashed") +
                geom_line(aes(y=pred.upr),color="black",linetype="dashed") +
                labs(x="log10(Relative abundance in oral cavity)",y="Frequency in SI") +
                scale_x_continuous(limits = c(-6.5,0))
        p1 <- p + geom_point(aes(y=freq,fill=tax_type),color="grey50",pch=21,size=.6,position="jitter",stroke=.5,alpha=.5) +
                 scale_fill_manual(values=tax_color) +
                 annotate("text",x=-5.8,y=0.96,label=paste("R2 = ",round(model.stat$Rsqr,4),sep="")) +
                 annotate("text",x=-5.8,y=0.89,label=paste("m = ",round(model.stat$m,4),sep="")) +
                 annotate("text",x=-5.8,y=0.82,label=paste("se = ",round(model.stat$m.se,4),sep=""))
        p2 <- p1 + mytheme + theme(legend.position="none")
        return(p2)
}
