library(data.table)
load("lopex.invert.RData")
load("lopex.RData")

# Convert lopex results to data.table and add sample_id
lopex.results.dt <- as.data.table(lopex.results)
lopex.results.dt[, sample_id := rownames(lopex.results)]

# Merge with chemistry data
setkey(lopex.results.dt, sample_id)
setkey(lopex.dat, spec_id)
lopex <- lopex.dat[lopex.results.dt]

# Some results plots
png("lopex.validate.png", width=4, height=6, units="in", res=200)
par(mfrow=c(3,2), pch=4)
with(lopex,{
    plot(N.mu, N)
    abline(0,1)
    plot(Cab.mu, C_ab)
    abline(0,1)
    plot(Car.mu, C_car)
    abline(0,1)
    plot(Cw.mu, EWT)
    abline(0,1)
    plot(Cm.mu, LMA)
    abline(0,1)
})
dev.off()


colnames(lopex)
#  [1] "common_name"      "latin_name"       "Refl_file"        "Trans_file"      
#  [5] "plant_type_lopex" "N"                "C_a"              "C_b"             
#  [9] "C_ab"             "C_car"            "C_anth"           "EWT"             
# [13] "LMA"              "FW"               "DW"               "A"               
# [17] "LT"               "C_C"              "C_H"              "C_O"             
# [21] "C_N"              "C_prot1"          "C_prot2"          "C_cell1"         
# [25] "C_cell2"          "C_lign1"          "C_lign2"          "C_star1"         
# [29] "C_star2"          "project"          "sample_year"      "sample_name"     
# [33] "spec_id"          "sample_id"        "N.mu"             "Cab.mu"          
# [37] "Car.mu"           "Cw.mu"            "Cm.mu"            "residual.mu"     
# [41] "N.sigma"          "Cab.sigma"        "Car.sigma"        "Cw.sigma"        
# [45] "Cm.sigma"         "residual.sigma"  
