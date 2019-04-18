function result = TGAB_pdf_Mixture_Single(x,p,mu,sigma,x_min)
    
    global xDomain;
    global pseudo_uniform;
    
    %[p mu sigma x_min]
   
    heaviside_l = @(x) 1.0*(x>=0);
    truncNormPDF = @(x,mu, sigma, x_min) (normpdf(x , mu, sigma)./normcdf(-x_min, -mu, sigma) .* heaviside_l(x - x_min));

    pseudo_normal = truncNormPDF(xDomain,mu,sigma,x_min).*pseudo_uniform;
    
    normFactor_uniform = sum(pseudo_uniform);
    normFactor_normal = sum(pseudo_normal);
    
    if normFactor_uniform == 0
        normFactor_uniform = 10^-8;
    end
    
    if normFactor_normal == 0
        normFactor_normal = 10^-8;
    end
    
    
    uniResultTemp = interp1(xDomain, pseudo_uniform, x);
    truncNormPDF(x, mu, sigma, x_min);
    normResultTemp = truncNormPDF(x,mu,sigma,x_min).*uniResultTemp;
    
    uniResultTemp = uniResultTemp/normFactor_uniform;
    normResultTemp = normResultTemp/normFactor_normal;
    
    propNorm = p;
    propUniform = 1-p;
    
    normResult = propNorm*normResultTemp;
    uniResult = propUniform*uniResultTemp;
    
    if sum(size(normResult)==size(uniResult))==2
        tempResult = normResult+uniResult;
    else
        tempResult = normResult+uniResult';
    end
    
    %xIndex = x-min(xDomain)+1;
    %results = tempResult(xIndex);
    result = tempResult;

end