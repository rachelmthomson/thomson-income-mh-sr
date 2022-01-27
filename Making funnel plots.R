source("Creating databases.R")

source("Running MAs.R")

funnel(models_beta$ma1a,
       xlab = "Standardised β")
metabias(models_beta$ma1a)

dev.copy2pdf(file = paste0("Funnel plots/ma1a.pdf"),
             width = 5,
             height = 3.5)

dev.copy(svg,
         file = paste0("Funnel plots/ma1a.svg"),
         width = 5,
         height = 3.5)
dev.off()

funnel(models_beta$ma1b,
       xlab = "Standardised β")
metabias(models_beta$ma1b)

dev.copy2pdf(file = paste0("Funnel plots/ma1b.pdf"),
             width = 5,
             height = 3.5)

dev.copy(svg,
         file = paste0("Funnel plots/ma1b.svg"),
         width = 5,
         height = 3.5)
dev.off()

funnel(models_smd$ma2a,
       xlab = "Standardised Mean Difference")
metabias(models_smd$ma2a)

dev.copy2pdf(file = paste0("Funnel plots/ma2a.pdf"),
             width = 5,
             height = 3.5)

dev.copy(svg,
         file = paste0("Funnel plots/ma2a.svg"),
         width = 5,
         height = 3.5)
dev.off()

funnel(models_smd$ma2b)
metabias(models_smd$ma2b,
         xlab = "Standardised Mean Difference")

dev.copy2pdf(file = paste0("Funnel plots/ma2b.pdf"),
             width = 5,
             height = 3.5)

dev.copy(svg,
         file = paste0("Funnel plots/ma2b.svg"),
         width = 5,
         height = 3.5)
dev.off()

funnel(models_smd$ma3a,
       xlab = "Standardised Mean Difference")
metabias(models_smd$ma3a)

dev.copy2pdf(file = paste0("Funnel plots/ma3a.pdf"),
             width = 5,
             height = 3.5)

dev.copy(svg,
         file = paste0("Funnel plots/ma3a.svg"),
         width = 5,
         height = 3.5)
dev.off()
