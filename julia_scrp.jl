using DataFrames
using RCall

#R模式下
parent_geno = fread("final.parent_genoPlink.ped")
offsp_geno =fread("final.off_genoPlink.ped")

#julia模式下
 parent_geno = @rget(parent_geno)
 offsp_geno = @rget(offsp_geno)

#SNPid用于存储哪些SNP保留
SNPId = Vector{Bool}(undef,294921)
for x=1:294921
 index = []
 #子代所有基因型
 Fall_g = Vector{String}(undef,233)
 #子代所有两个等位基因
 F1 = []
   for i=1:233
   #子代的双亲
    P1 = split(offsp_geno[i,1],"_")[1]*"_"
    P2 = split(offsp_geno[i,1],"_")[2]*"_"
    #亲本1基因型
    P1_g1 = parent_geno[occursin.(Regex("^$P1"),parent_geno[:,1]),6+(2*x-1)][1]
    P1_g2 = parent_geno[occursin.(Regex("^$P1"),parent_geno[:,1]),6+(2*x)][1]
    #亲本2基因型
    P2_g1 = parent_geno[occursin.(Regex("^$P2"),parent_geno[:,1]),6+(2*x-1)][1]
    P2_g2 = parent_geno[occursin.(Regex("^$P2"),parent_geno[:,1]),6+(2*x)][1]
    #子代所有可能基因型
    Pall_g = collect(Set([P1_g1*P2_g1,P1_g1*P2_g2,P2_g1*P1_g1,P2_g1*P1_g2,P2_g2*P1_g1,P2_g2*P1_g2,P1_g2*P2_g1,P1_g2*P2_g2]))
    #子代实际两个等位基因
    F1_g1 = offsp_geno[i,6+(2*x-1)]
	F1_g2 = offsp_geno[i,6+(2*x)]
	#子代实际基因型
	F1_g = F1_g1*F1_g2
	push!(F1,F1_g1,F1_g2)
	
    Fall_g[i] = F1_g
	#判断子代观测基因型是否满足亲本分离规律
    judge = F1_g in Pall_g
    push!(index,judge)
   end
 #统计子代两个等位基因的个数用于计算MAF
 g1 = count(F1.==collect(Set(F1))[1])
 g2 = count(F1.==collect(Set(F1))[2])
 if(g1<g2)
    MAF = g1/length(F1)
 else
    MAF = g2/length(F1)
 end
 #如果该SNP满足亲本分离规律，基因型类型两种以上，且MAF大于0.05，则保留该SNP
 if(all(index)&&(length(Set(Fall_g))!==1)&&(MAF>0.05))
  SNPId[x] = true
 else
   SNPId[x] = false
 end
end
 
 
 