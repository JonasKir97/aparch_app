#include <Rcpp.h>
#include <vector>
#include <math.h>
#include <map>

using namespace Rcpp;

double h(double x, double g, double d) {
  return pow(abs(x)-g*x,d);
}

// [[Rcpp::export]]
std::map<std::string, std::vector<double>> simulateDiscreteAPARCH11inCPP(std::vector<double> noises, 
                                                                         double alpha, double beta, double theta, double gamma, double delta, 
                                                                         double initialSigmaDelta, double initialY) {
  
  std::map<std::string, std::vector<double>> resultMap;
  
  std::vector<double> sigmaDeltas;
  sigmaDeltas.push_back(initialSigmaDelta);
  std::vector<double> Y;
  Y.push_back(initialY);
  
  double oneOverDelta = 1/delta;
  double newSigDelta;
  
  int ns = noises.size();
  
  for(int k=0;k<ns;k++) {
    newSigDelta = theta + alpha*h(Y[k],gamma,delta) + beta * sigmaDeltas[k];
    sigmaDeltas.push_back(newSigDelta);
    Y.push_back(noises[k]*std::pow(newSigDelta,oneOverDelta));
  }
  
  resultMap["sigmaDelta"] = sigmaDeltas;
  resultMap["Y"] = Y;
  return resultMap;
}

