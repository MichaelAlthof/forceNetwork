
library(R.matlab)
library(networkD3)

setwd("c:/r")

mat_obj = readMat("adjacency_matrix.mat")
# get Matrix from Matlab object
A = mat_obj$BinAdj
dim(A)

B = A[1:5,1:5]

# created by Lukas Borke
create_forceNetwork_from_adjacency_matrix = function(A) {

	d.names = 1:nrow(A)
	d.group = rep(1, nrow(A))
	(Nodes = data.frame(name = d.names, group = d.group))

	s_v = vector()
	t_v = vector()
	v_v = vector()

	for (i in (1:nrow(A))) {
		line_i = A[i,]
		t = which(line_i > 0)
		active_s = length(t)
		if (active_s > 0) {
			s_v = c(s_v, rep((i-1), active_s))
			t_v = c(t_v, (t-1) )
			v_v = c(v_v, rep(1, active_s))
		}
	}

	(Links = data.frame(source = s_v, target = t_v, value = v_v))

	FN = forceNetwork(Links = Links, Nodes = Nodes, Source = "source",
	Target = "target", Value = "value", NodeID = "name",
	Group = "group", opacity = 1, zoom = TRUE)

	return(FN)
}

FN1 = create_forceNetwork_from_adjacency_matrix(A)

FN2 = create_forceNetwork_from_adjacency_matrix(B)

# show the interactive graph
FN1
FN2
