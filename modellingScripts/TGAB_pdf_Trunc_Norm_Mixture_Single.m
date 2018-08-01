function result = TGAB_pdf_Mixture_Single(x,p,mu,sigma)
    
    global xDomain;
    global pseudo_uniform;
    
    truncNormPDF = @truncated_normal_a_pdf %truncated at lower bound a
    a = -.01 %Lower bound should give us results at 0

    pseudo_normal = truncNormPDF(xDomain,mu,sigma,a).*pseudo_uniform;
    
    normFactor_uniform = sum(pseudo_uniform);
    normFactor_normal = sum(pseudo_normal);
    
    if normFactor_uniform == 0
        normFactor_uniform = 10^-8;
    end
    
    if normFactor_normal == 0
        normFactor_normal = 10^-8;
    end
    
    
    uniResultTemp = interp1(xDomain, pseudo_uniform, x);
    normResultTemp = truncNormPDF(x,mu,sigma,a).*uniResultTemp;
    
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