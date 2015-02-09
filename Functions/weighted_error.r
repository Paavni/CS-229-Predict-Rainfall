weighted_error = function(pred, actual)
{
sume = 0
for(a in 1:dim(actual)[1])
{
	if(actual[a]==0)
	{
	sume = (abs(pred[a]-actual[a]))*0.000001
	}
	else
	{
	sume = (abs(pred[a]-actual[a]))*actual[a]
	}
}
sume = sume/sum(actual)
return(sume)
}