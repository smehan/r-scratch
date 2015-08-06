# multiple plots via a for, with the x = sprintf in the aes call providing contextualized strings as params

varList = list("Var1","Var2","Var3")
plot_list = list()
for (i in 1:3) {
    gg = ggplot(data_set,aes(xfill=factor(RETAINED)))
    gg = gg + aes_string(x = sprintf("log(%s)", varList[[i]])) 
    gg = gg + geom_density(alpha=.3) + labs(x = varList[[i]],y="Density") 
    gg = gg + ggtitle(paste("Distribution of ",varList[[i]],sep=" ")) 
    plot_list[[i]] = gg
}
