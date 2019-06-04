#include <Rcpp.h>
#include <iostream>



// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

NumericVector Cquantile(NumericVector& x, NumericVector& q) {
  NumericVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y[x.size()*(q - 0.000000001)];
}

//' @title  Calcula um intervalo de confiança Bootstrap para diferença de médias
//'
//' @description Utilizando dois vetores \code{numeric} calcula-se o intervalo de
//' Bootstrap de diferença absoluta de médias conforme quantis fornecidos.
//'
//' @param tratamento Um vetor \code{numeric} com o quantidade estudada observada
//' no tratamento
//' @param controle Um vetor \code{numeric} com a quantidade estudada observada
//' no grupo controle
//' @param B_size Um número \code{integer} com o tamanho desejado para a reamostragem
//' Boostrap
//' @param replicas Um número \code{integer} com a quantidade de replicas (simulações)
//' Bootstrap que serão feitas
//' @param inf Um número \code{numeric} informando o quantil inferior do intervalo
//' @param sup Um número \code{numeric} informando o quantil inferior do intervalo


// [[Rcpp::export]]

Rcpp::List boot_mean_interval(NumericVector& tratamento,
                              NumericVector& controle,
                              int& B_size,
                              int& replicas,
                              float inf,
                              float sup){

  // Get vectors size to use in sample
  int tratamento_size = tratamento.size();
  int controle_size = controle.size();

  // vector of distribution of means
  NumericVector distribuicao(replicas);
  // creating a numericvector with the inf and sup parts of interval
  NumericVector interval = NumericVector::create(inf, sup);
  double media_tratamento;
  double media_controle;
  for(int i = 0; i < replicas; i++){
    // get a temporare objecto of class numeric vector of subset
    NumericVector tmp1 = tratamento[floor(Rcpp::runif(B_size,0,tratamento_size))];
    NumericVector tmp2 = controle[floor(Rcpp::runif(B_size,0,controle_size))];
    // calculating means
    media_tratamento = Rcpp::mean(tmp1);
    media_controle = Rcpp::mean(tmp2);
    // calculating the diference
    distribuicao[i] = media_tratamento - media_controle;
  }

  NumericVector intervalo = Cquantile(distribuicao, interval);
  double media = mean(distribuicao);

  return Rcpp::List::create(Rcpp::Named("inf")  = intervalo[0],
                            Rcpp::Named("media")  = media,
                            Rcpp::Named("sup") = intervalo[1]);

}



//' @title  Calcula um intervalo de confiança Bootstrap para diferença relativa de médias
//'
//' @description Utilizando dois vetores \code{numeric} calcula-se o intervalo de
//' Bootstrap de diferença relativa \eqn{ \frac{mean(tratamento)}{mean(controle)}} de médias, conforme quantis fornecidos.
//'
//' @param tratamento Um vetor \code{numeric} com o quantidade estudada observada
//' no tratamento
//' @param controle Um vetor \code{numeric} com a quantidade estudada observada
//' no grupo controle
//' @param B_size Um número \code{integer} com o tamanho desejado para a reamostragem
//' Boostrap
//' @param replicas Um número \code{integer} com a quantidade de replicas (simulações)
//' Bootstrap que serão feitas
//' @param inf Um número \code{numeric} informando o quantil inferior do intervalo
//' @param sup Um número \code{numeric} informando o quantil inferior do intervalo
//'
//' @export
//'
// [[Rcpp::export]]

Rcpp::List boot_mean_rel_interval(NumericVector& tratamento,
                              NumericVector& controle,
                              int& B_size,
                              int& replicas,
                              float inf,
                              float sup){

  // Get vectors size to use in sample
  int tratamento_size = tratamento.size();
  int controle_size = controle.size();

  NumericVector distribuicao(replicas);
  NumericVector interval = NumericVector::create(inf, sup);
  double media_tratamento;
  double media_controle;
  for(int i = 0; i < replicas; i++){

    NumericVector tmp1 = tratamento[Rcpp::floor(Rcpp::runif(B_size,0,tratamento_size))];
    NumericVector tmp2 = controle[Rcpp::floor(Rcpp::runif(B_size,0,controle_size))];
    media_tratamento = Rcpp::mean(tmp1);
    media_controle = Rcpp::mean(tmp2);
    distribuicao[i] = (media_tratamento) / (media_controle);
  }

  NumericVector intervalo = Cquantile(distribuicao, interval);
  double media = mean(distribuicao);

  return Rcpp::List::create(Rcpp::Named("inf")  = intervalo[0],
                            Rcpp::Named("media")  = media,
                            Rcpp::Named("sup") = intervalo[1]);

}



//' @title  Calcula um intervalo de confiança Bootstrap para diferença relativa de proporções
//'
//' @description Utilizando dois vetores \code{numeric} calcula-se o intervalo de
//' Bootstrap de diferença relativa \eqn{ \frac{mean(tratamento)}{mean(controle)}} das proporções, conforme quantis fornecidos.
//'
//' @param tratamento Um vetor \code{numeric} com o quantidade estudada observada
//' no tratamento
//' @param controle Um vetor \code{numeric} com a quantidade estudada observada
//' no grupo controle
//' @param B_size Um número \code{integer} com o tamanho desejado para a reamostragem
//' Boostrap
//' @param replicas Um número \code{integer} com a quantidade de replicas (simulações)
//' Bootstrap que serão feitas
//' @param inf Um número \code{numeric} informando o quantil inferior do intervalo
//' @param sup Um número \code{numeric} informando o quantil inferior do intervalo
//'
//' @export
//'
// [[Rcpp::export]]

Rcpp::List boot_prop_interval(NumericVector& tratamento,
                              NumericVector& controle,
                              int& B_size,
                              int& replicas,
                              float inf,
                              float sup){

  // Get vectors size to use in sample
  int tratamento_size = tratamento.size();
  int controle_size = controle.size();

  NumericVector distribuicao(replicas);
  NumericVector interval = NumericVector::create(inf, sup);
  double media_tratamento;
  double media_controle;
  for(int i = 0; i < replicas; i++){

    NumericVector tmp1 = tratamento[Rcpp::floor(Rcpp::runif(B_size,0,tratamento_size))];
    NumericVector tmp2 = controle[Rcpp::floor(Rcpp::runif(B_size,0,controle_size))];
    media_tratamento = Rcpp::mean(tmp1);
    media_controle = Rcpp::mean(tmp2);
    distribuicao[i] = (media_tratamento) / (media_controle);
  }

  NumericVector intervalo = Cquantile(distribuicao, interval);
  double media = mean(distribuicao);

  return Rcpp::List::create(Rcpp::Named("inf")  = intervalo[0],
                            Rcpp::Named("media")  = media,
                            Rcpp::Named("sup") = intervalo[1]);

}
