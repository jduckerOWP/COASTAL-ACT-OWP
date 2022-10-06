% Sample calling syntax for testing taopounders and comparing to fminsearch
% TestingInitialize is called once before solving any instances

% Make the test problem accessible to the matlab session and load the data matrix
addpath('more_wild_probs/')
load dfo.dat

% Allocate global vectors to store the iterates generated by the algorithms
global nfev fvals fvecs X_hist 

% Initialize the problems that are being solved and the storage of the results
to_solve = 1:53;
Results = cell(1,length(to_solve));

% Initialize the constant factor for the number of allowable function evaluations
nf_const = 10;

