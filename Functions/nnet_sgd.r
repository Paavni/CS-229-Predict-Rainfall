nnet_sgd = function(city,k=3,s=20,decay=0.00001, maxit = 1000, window = FALSE, past = TRUE, future = TRUE)
{
p = rep(0, length(dim(city)[1]))
years = ceiling(dim(city)[1] / 12)
for(i in (1:dim(city)[1]))
{
	x = city[i,c(1:10)];
	y = matrix(city[i,11]);
	if(window)
	{
	j = k%/%2;
	if(i-j<1)
	{
		x = city[c(i:(i+j)),c(1:10)];
		y = matrix(city[c(i:(i+j)),11]);
		for(a in 1:j)
		{
			if(i-a>=1)
			{
				x = rbind(city[i-a,c(1:10)],x);
				y = rbind(city[i-a,11],y);
			}
			else
			{
				x = rbind(meancity[c(1:10)],x);
				y = rbind(meancity[11],y);
			}
		}
	}
	if(i+j>dim(city)[1])
	{
		x = city[c((i-j):i),c(1:10)];
		y = matrix(city[c((i-j):i),11]);
		for(a in 1:j)
		{
			if(i+a<=dim(city)[1])
			{
				x = rbind(x,city[i+a,c(1:10)]);
				y = rbind(y,city[i+a,11]);
			}
			else
			{
				x = rbind(x,meancity[c(1:10)]);
				y = rbind(y,meancity[11]);
			}
		}
	}
	if(i-j>=1 & i+j<=dim(city)[1])
	{
		x = city[c((i-j):(i+j)),c(1:10)]
		y = matrix(city[c((i-j):(i+j)),11]);
	}
	}	
	if(future)
	{
	for(l in 1:years)
	{
		i_row = i+(l*12)
				if(i_row > dim(city)[1])
			{
				break
			}
		x = rbind(x,city[i_row,c(1:10)]);
		y = rbind(y,matrix(city[i_row,11]));
	}	
	}
	if(past)
	{
	for(l in 1:years)
	{
		i_row = i-(l*12)
				if(i_row < 1)
			{
				break
			}
		x = rbind(city[i_row,c(1:10)],x);
		y = rbind(matrix(city[i_row,11]),y);
	}		
	}
	if(i>1)
	{
		n = nnet(x, y, size = s, Wts = wts, linout = TRUE, maxit = maxit, decay = decay);
	}
	else
	{
		# epsilon = sqrt(6)/sqrt(s+(dim(x)[2]*k))
		epsilon = 0.5
		n = nnet(x, y, size = s, linout = TRUE, maxit = maxit, decay = decay, rang = epsilon);
	}
	p[i] = predict(n, city[i,c(1:10)], type="raw");
	wts = n$wts;
}
# rmae = mean(abs((p[city[,11]!=0]-city[city[,11]!=0,11])/city[city[,11]!=0,11]))
# rmse = sqrt(mean(((p[city[,11]!=0]-city[city[,11]!=0,11])/city[city[,11]!=0,11])^2))
# mae = mean(abs(p[city[,11]==0]-city[city[,11]==0,11]))
# mse = sqrt(mean((p[city[,11]==0]-city[city[,11]==0,11])^2))
ae = mean(abs(p-city[,11]))
return(list(n=n, p=p,ae = ae))
}

