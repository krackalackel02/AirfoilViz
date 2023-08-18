function [mu,Cl] = finalcalc(xi,zi,uinf)
% FINALCALC() is a nested loop function made to calculate values of panel strength and coefficient of lift
% 
% *VARIABLES:*
% 
% * *centrepoints* - Centre points between each panel point
% * *beta* - Angle the panel makes relative to x-axis
% * *uij* - Horizontal velocitites imparted at the centre of the ith panel by the jth panel
% * *vij* - Vertical velocitites imparted at the centre of the ith panel by the jth panel
% * *A* - Matrix that gets multiplied by panel strengths to provides N+1 equations
% * *B* - Solutions for the N+1 equations
% * *mu* - Panel strengths column vector
% * *Cl* - Coefficient of lift calculated with the panel strength of the wake panel
% 
% Using array operators at every step to reduce run time of code except for
% when using CDOUBLET() since its limited by its output so must be used
% within for loop
% 
% Making centre points by summing the first n-1 x & z values with the last
% n-1 x & z values and dividing by 2
% 
% Using atan2() allows for beta to be within range of -pi and pi which is
% equivalent to 0 and 2pi
% 
% Outer loop runs from 1:N and inner loop from 1:n+1 to make N relations
% involving N+1 terms/panel strengths to solve for uij and vij
% 
% Alpha is converted into radians and subbed into equation 12 ad then
% rearranged to solve for panel strengths
% 
% The N+1 relation/equation provided from kutta condition about circulation
% being 0 which forms a N+1xN+1 matrix which we can now take the inverse of
    
    n = length(xi)-2;
    alpha = atand(zi(n+2)/(xi(n+2)-xi(n+1)));
   
    % Making centre point
    centrepoints = [(((xi(2:n+2))+(xi(1:n+1)))/2),;(((zi(2:n+2))+(zi(1:n+1)))/2)]';
    
    % atan2(dz/dx)
    beta = (atan2((zi(2:n+1)-zi(1:n)),(xi(2:n+1)-xi(1:n))))';
    [uij,vij] = deal(zeros(n,n+1));
   
    % Calculating uij and vij matrices
    for ucount = 1:n

        for mucounter = 1:n+1

            [uij(ucount,mucounter),vij(ucount,mucounter)] = cdoublet(centrepoints(ucount,:),[xi(mucounter),zi(mucounter)],[xi(mucounter+1),zi(mucounter+1)]);

        end

    end
   
    % Equation 12 from handout provides following n relations for A and B
    A = vij.*(cos(beta)*ones(1,n+1))-uij.*(sin(beta)*ones(1,n+1));
    B = -uinf*ones(n,1).*sin(-beta+(alpha*pi/180));
    
    
   % Relation n+1 provided from kutta condition in equation 13
    A(n+1,[n+1,1,n]) = [1,1,-1];
    B(n+1) = 0;
    mu = A\B;
    Cl = -2*mu(n+1)/uinf;

end