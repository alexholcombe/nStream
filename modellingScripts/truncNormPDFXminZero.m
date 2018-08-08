function result = truncNorm(x,mu,sigma)	
	[mu sigma]
	x_min = -.1;
	heaviside_l = @(x) 1.0*(x>=0);
    truncNormPDF = (normpdf(x , mu, sigma)./normcdf(-x_min, -mu, sigma) .* heaviside_l(x - x_min));
    result = truncNormPDF;
end