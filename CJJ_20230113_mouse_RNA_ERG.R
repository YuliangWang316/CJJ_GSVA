setwd("d:/")
diff_gene <- read.table("Merge_ERG_All_result_20230113_mouse.txt", sep="\t", header=TRUE, row.names=1)
diff_gene=as.data.frame(diff_gene)
gene_list=diff_gene[,c("log2FoldChange","padj")]

colnames(gene_list)=c("logFC","padj")
gene_list$threshold = as.factor(abs(gene_list$logFC) > 1 & gene_list$padj < 0.05)
colored_point<-gene_list[gene_list$threshold == "TRUE",]
Spgenes<-colored_point[rownames(colored_point) == "KDM6B" ,]
gene_list$threshold<-as.character(gene_list$threshold)
gene_list$threshold[which(rownames(gene_list) == "KDM6B" )]<-"KDM6B"
gene_list$threshold[which(gene_list$logFC >1 & gene_list$threshold ==TRUE)] <- "UP"
colnames(gene_list)[3]<-"Significant"
gene_list$Significant[which(gene_list$Significant == "TRUE")]<-"Down"
gene_list$Significant[which(gene_list$Significant == "FALSE")]<-"Not Sig"
Mycolors<-c("Blue","Black","Gray","Red")
library("ggplot2")
pdf("vocano.pdf")

g = ggplot(data=gene_list, aes(x=logFC, y=-log10(padj),color=Significant)) + geom_point(alpha=0.8, size=1.75)  + xlim(c(-4, 4)) + ylim(c(0, 50)) +xlab("log2 fold change") + ylab("-log10 p-value") + theme_set(theme_bw()) + theme(panel.grid.major=element_line(colour=NA)) + scale_color_manual(values = Mycolors) + geom_text(mapping=aes(label=rownames(Spgenes)),data = Spgenes,check_overlap = TRUE,color="Black")
print(g)
dev.off()
