generic 
	
	N : Integer;

package Algebre is

	type T_Vecteur is array (1 .. N) of float;
	type T_Matrice is array (1 .. N) of T_Vecteur;

	-- Initialise un vecteur avec une même coordonnée
	procedure Initialiser_Vecteur(V : in out T_Vecteur; Coordonnee : in float);

	-- Initialise une matrice avec une même coordonnée
	procedure Initialiser_Matrice(M : in out T_Matrice; Coordonnee : in float);

	-- Calcule et renvoie le produit d'un vecteur V et d'une matrice M
	function Produit_Vecteur_Matrice(V : in T_Vecteur; M : in T_Matrice) return T_vecteur;

	-- Calcule et renvoie le produit d'un scalaire a et d'une matrice M
	procedure Produit_Scalaire_Matrice(a : in float; M : in out T_Matrice);

	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice M
	function Somme_Ligne(M : in T_Matrice; Ligne_i : in Integer) return float;

	-- Calcule et renvoie la somme de deux matrices
	function Somme_Matrice(M1 : in T_Matrice; M2 : in T_Matrice) return T_Matrice;

	-- Verifie si la ligne i est vide
	function Ligne_Vide(M : in T_Matrice; Ligne_i : in Integer) return Boolean;

	-- Normalise la ième ligne de M
	procedure Normaliser_Ligne(M : in out T_Matrice; Ligne_i : in Integer);

	-- Trier PageRank et les pages en fonction du PageRank
	procedure Trier(Poids : in out T_Vecteur; PageRank : in out T_Vecteur; debut : in Integer; fin : in Integer);

end Algebre;
