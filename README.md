# Code for *Ubiquity of synonymity: almost all large binary trees are not uniquely identified by their spectra or their immanantal polynomials* by Matsen and Evans

[Link to paper] (http://arxiv.org/abs/q-bio/0512010)

## Abstract

There are several common ways to encode a tree as a matrix, such as the adjacency matrix, the Laplacian matrix (that is, the infinitesimal generator of the natural random walk), and the matrix of pairwise distances between leaves. Such representations involve a specific labeling of the vertices or at least the leaves, and so it is natural to attempt to identify trees by some feature of the associated matrices that is invariant under relabeling. An obvious candidate is the spectrum of eigenvalues (or, equivalently, the characteristic polynomial). We show for any of these choices of matrix that the fraction of binary trees with a unique spectrum goes to zero as the number of leaves goes to infinity. We investigate the rate of convergence of the above fraction to zero using numerical methods. For the adjacency and Laplacian matrices, we show that that the *a priori* more informative immanantal polynomials have no greater power to distinguish between trees.

## Code and results

### `immanantal` directory: for immanantal polynomial of generalized Laplacian spectrum
* `immanantal/check_results` - checks to make sure that the trees in Figure 1(b)
   have the exchange property with respect to the immanantal polynomials
* `immanantal/coimm_results` - coimmanantal pairs of trees
* `immanantal/n_spectra_results` - number of immanantal polynomials

This empty diff shows that the hypotheses of Lemma 3 are satisfied for
the trees in Figure 1(b):

    diff immanantal/check_results/a immanantal/check_results/b

### `distance` directory: distance matrix spectrum
* `distance/check_results`- checks to make sure that the trees in Figure 1(b)
   have the exchange property with respect to the distance matrix spectrum
* `distance/n_spectra_results` - the number of spectra

This empty diff shows that the hypotheses of Lemma 4 are satisfied for
the trees in Figure 1(b):

    diff distance/check_results/dist_poly1.txt distance/check_results/dist_poly2.txt

