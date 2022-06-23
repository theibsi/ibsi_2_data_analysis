function gM = createGammaMask(RM,gammaV)
gM = zeros(size(RM));
gM(RM<=gammaV)= 1;
end