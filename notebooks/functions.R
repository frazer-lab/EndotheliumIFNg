
extract_coxme_table <- function (mod){
    beta <- mod$coefficients$fixed
    nvar <- length(beta)
    nfrail <- nrow(mod$var) - nvar
    se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
    z<- beta/se
    p<- pchisq((z)^2, 1, lower.tail = FALSE)
    table=data.frame(cbind(beta,se,z,p))
    return(table)
}

psize = function(height, width) 
{
    options(repr.plot.height=height, repr.plot.width=width)
}

my.invnorm = function(x)
{
    res = rank(x)
    res = qnorm(res/(length(res)+0.5))
    return(res)
}

transform_standard_normal = function(df)
{
    data_valid_expressed_full_qn = normalize.quantiles(as.matrix(df), copy=FALSE)

    input_mat = as.data.frame(t(apply(t(data_valid_expressed_full_qn), 2, my.invnorm)))
    
    return(input_mat)
}

addDashUUID = function(uuid)
{
    splitted = unlist(strsplit(uuid, ""))
    out = paste(paste(splitted[ 1: 8], collapse = ""),
                paste(splitted[ 9:12], collapse = ""),
                paste(splitted[13:16], collapse = ""),
                paste(splitted[17:20], collapse = ""),
                paste(splitted[21:32], collapse = ""),
                sep = "-"
               )
    return(out)
}

add_rownames = function(x) # add rownames to fread
{
	rownames(x) = x[,1]
	x[,1]       = NULL
	return(x)
}

createColorPanel <- function(num.color){
	colPanel = c(
		"grey", "#E31A1C", "#FFD700", "#771122", "#777711", "#1F78B4", "#68228B", "#AAAA44",
		"#60CC52", "#771155", "#DDDD77", "#774411", "#AA7744", "#AA4455", "#117744",
		"#000080", "#44AA77", "#AA4488", "#DDAA77", "#D9D9D9", "#BC80BD", "#FFED6F",
	    "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17",
	    "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
	    "#A6761D", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
	    "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928", "#FBB4AE", "#B3CDE3",
	    "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2",
	    "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC",
	    "#CCCCCC", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FFFF33", "#A65628",
	    "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
	    "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
	    "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5"
	   )
}
