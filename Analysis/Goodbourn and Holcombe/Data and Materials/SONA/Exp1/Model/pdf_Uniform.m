function result = TG_pdf_Uniform(x,p)
    
    %fprintf('\np %0.2f, mu1 %2.2f, sigma1 %2.2f', p, mu1, sigma1);
    % Need to taper the normal distribution with the pseudouniform
    
    global xDomain;
    global pseudo_uniform;
    
    normFactor_uniform = sum(pseudo_uniform);
    
    if normFactor_uniform == 0
        normFactor_uniform = 0.00001;
    end
    
    uniResultTemp = interp1(xDomain,pseudo_uniform,x);
    result = p*(uniResultTemp/normFactor_uniform);

end