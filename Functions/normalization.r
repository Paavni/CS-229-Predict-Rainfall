normalization = function(M)	
{
	return ((M-min(M))/(max(M)-min(M)))
}

de_normalization = function(N, M)	
{

	return ((N+min(M))*(max(M)-min(M)))

}