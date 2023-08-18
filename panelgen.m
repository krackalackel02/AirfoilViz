function [xi,zi] = panelgen(NACA,n,alpha)
% PANELGEN() is a function to generate and discretize airfoil
% 
% *VARIABLES:*
% 
% * *NACA* - Designation for the chosen NACA 4-series airfoil
% * *m* - Maximum camber relative to chord length 
% * *p* - Location of max camber relative to chord length
% * *t* - Max airfoil thickness relative to chord length which occurs at 0.3c
% * *n* - Number of panel points including the wake panel
% * *alpha* - Max airfoil thickness relative to chord length which occurs at 0.3c
% * *xc* - X values for panel points of mean camber line
% * *yc* - Z values for panel points of mean camber line
% * *yt* - Thickness values for each x point
% * *dyc_dxc* - Gradients of mean camber line
% * *theta* - Angle the components of thickness are applied with
% * *xu* - X values for panel points of airfoil's upper surface
% * *zu* - Z values for panel points of airfoil's upper surface
% * *xl* - X values for panel points of airfoil's lower surface
% * *zl* - Z values for panel points of airfoil's lower surface
% * *xi* - X values for panel points from trailing edge to leading edge and back to trailing edge
% * *zi* - Z values for panel points from trailing edge to leading edge and back to trailing edge
% 
% Using str2double of NACA array elements and dividing by 10 or 100 to get
% airfoil geometry
% 
% Using array operators at every step to reduce run time of code
% 
% Using equation 7 we get the distribution of panel end points
% 
% Using equation 1 we get the formula for mean camber line
% 
% Using equation 2 we get values of thickness across chord
%
% Using equation 3 and 4 we see how to apply thickness to camber line to
% form upper and lower sufaces of airfoil
% 
% Using equation 5 and 6 we see how to calulate angle each panel makes
% respective to x axis
% 
% Applying wake panel by using simple trigonometry where
% opposite=adjacent*tan(angle)

    % Defining aerofoil characteristics
    NACA = num2str(NACA);
    m = (str2double(NACA(1)))/100;
    p = (str2double(NACA(2)))/10;
    t = (str2double(NACA(3)))/10+(str2double(NACA(4)))/100;

    % Finding mean camber line
    [xc] = 1-0.5.*(1-cos(2.*pi.*(((1:n+1)-1)./n)));
    yc(xc <= p) = (m./((p).^2)).*(2.*(p).*xc(xc <= p)-(xc(xc <= p)).^2);
    yc(xc >= p) = (m./((1-p).^2)).*((1-2.*p)+2.*p.*xc(xc >= p)-(xc(xc >= p)).^2);

    % Calculating thickness
    yt = 5.*t.*(0.2969.*sqrt(xc)-0.126.*xc-0.3516.*(xc).^2+0.2843.*(xc).^3-0.1015.*(xc).^4);

    % Calulating thetas
    dyc_dxc(xc <= p) = (2.*m./((p).^2)).*((p)-(xc(xc <= p)));
    dyc_dxc(xc >= p) = (2.*m./((1-p).^2)).*((p)-(xc(xc >= p)));
    theta = atan(dyc_dxc);
   
    % Applying thickness
    xu(1:n/2+1) = xc(n/2+1:n+1)-yt(n/2+1:n+1).*sin(theta(n/2+1:n+1));
    zu(1:n/2+1) = yc(n/2+1:n+1)+yt(n/2+1:n+1).*cos(theta(n/2+1:n+1));
    xl(1:n/2+1) = xc(1:n/2+1)+yt(1:n/2+1).*sin(theta(1:n/2+1));
    zl(1:n/2+1) = yc(1:n/2+1)-yt(1:n/2+1).*cos(theta(1:n/2+1));
   
    % Correction for leading and trailing edge
    [xu(1),zu(1),xl(n/2+1),zl(n/2+1),zu(n/2+1),zl(1)] = deal(0);
    [xu(n/2+1),xl(1)] = deal(1);
   
    % Final collection of panel points after adding wake panel and
    % concatenating
    xi = [xl,xu(2:end),9e99];
    zi = [zl,zu(2:end),9e99*tand(alpha)];
    
end