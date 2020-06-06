#--------------------------------------------------------------------------
#  Description:                                                             
#     This routine approximates the solution of the differential equation
#     y'=f(t,y) with the initial condition y = y0 at t = t0. The value at
#     t + dt is returned in y. Uses fourth order Runge-Kutta method.
#                                                                           
#  Arguments:    
#     param    Parameters of deriv function which returns the slope at 
#              (t,y) of integral curve of the differential equation 
#              y' = f(t,y)  			                    
#     t        Initial value of t.                                        
#     dt       Step size                                                  
#     y        On input y is the initial value of y at t, on output y is  
#	             the solution at t + dt.                              		
#
#  Return Values:
#     y        Solution of y(t) at t + dt
#--------------------------------------------------------------------------
rk_4ordem <- function(param, t, dt, Y) {
  
  threshold <- 1.e-6 #The minimum number of detectable cells
  
  k1 = deriv(param, t, Y)
  k2 = deriv(param, t + 0.5*dt, Y + 0.5*dt*k1)
  k3 = deriv(param, t + 0.5*dt, Y + 0.5*dt*k2)
  k4 = deriv(param, t + dt    , Y + dt*k3)
  
  Y = Y + (dt/6.) * (k1 + 2*k2 + 2*k3 + k4)
  
  if (Y[1] < threshold) {Y[1] = 0.}
  if (Y[2] < threshold) {Y[2] = 0.}
  if (Y[3] < threshold) {Y[3] = 0.}
  
  result <- Y
}
	
#----------------------------------------------------------------------
# --- Model derivative function f(t,Y), where  
#        Y = (C_T, C_M, T)
#    param = (phi, rho, mu_T, theta, alfa, beta, mu_M, r, b, gamma)
#----------------------------------------------------------------------
deriv <- function(param, t, Y) {
  
  deriv_CT = (param[1]-param[2]-param[3])*Y[1] + param[4]*Y[3]*Y[2] - param[5]*Y[3]*Y[1]
  
  deriv_CM = param[6]*Y[1] - param[4]*Y[3]*Y[2] - param[7]*Y[2]
  
  deriv_T  = param[8]*Y[3]*(1.-param[9]*Y[3]) - param[10]*Y[3]*Y[1]
  
  result <- c(deriv_CT, deriv_CM, deriv_T)
}