generic 
	
	N : Integer;
	-- type reel de precision quelconque
	type T_Element is digits <>;

package Algebre is

	type T_Vecteur is array (1 .. N) of T_Element;
	type T_Matrice is array (1 .. N) of T_Vecteur;
		
	type T_Cellule;
	type T_LCA is access T_Cellule;    
	type T_Cellule is record        
		Colonne : Integer;
		Donnee : T_Element;
		Suivant : T_LCA;
	end record;
	type T_Matrice_Creuse is array (1..N) of T_LCA;

	-- Initialise un vecteur avec une même coordonnée
	procedure Initialiser_Vecteur(V : in out T_Vecteur; Coordonnee : in T_Element);

	-- Initialise une matrice avec une même coordonnée
	procedure Initialiser_Matrice(M : in out T_Matrice; Coordonnee : in T_Element);

	-- Calcule et renvoie le produit d'un vecteur V et d'une matrice M
	function Produit_Vecteur_Matrice(V : in T_Vecteur; M : in T_Matrice) return T_vecteur;

	-- Calcule et renvoie le produit d'un scalaire a et d'une matrice M
	procedure Produit_Scalaire_Matrice(a : in T_Element; M : in out T_Matrice);

	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice M
	function Somme_Ligne(M : in T_Matrice; Ligne_i : in Integer) return T_Element;

	-- Calcule et renvoie la somme de deux matrices
	function Somme_Matrice(M1 : in T_Matrice; M2 : in T_Matrice) return T_Matrice;

	-- Verifie si la ligne i est vide
	function Ligne_Vide(M : in T_Matrice; Ligne_i : in Integer) return Boolean;

	-- Normalise la ième ligne de M
	procedure Normaliser_Ligne(M : in out T_Matrice; Ligne_i : in Integer);

	-- Trier PageRank et les pages en fonction du PageRank
	procedure Trier(Poids : in out T_Vecteur; PageRank : in out T_Vecteur; debut : in Integer; fin : in Integer);
		
		
	-- Initialiser une Sda.  La Sda est vide.
	procedure Initialiser(Sda: out T_LCA) with
		Post => Est_Vide (Sda);

	-- Est-ce qu'une Sda est vide ?
	function Est_Vide (Sda : T_LCA) return Boolean;
		
	-- Enregistrer une Donnée associée à une Colonne dans une Sda.
	-- Si la Colonne est déjà présente dans la Sda, sa donnée est changée.
	procedure Enregistrer (Sda : in out T_LCA ; Colonne : in Integer ; Donnee : in T_Element);
		
	-- Obtenir la donnée associée à une Colonne dans la Sda.
	-- Exception : Cle_Absente_Exception si Colonne n'est pas utilisée dans l'Sda
	function La_Donnee (Sda : in T_LCA ; Colonne : in Integer) return T_Element;

	-- Supprimer la Donnée associée à une Colonne dans une Sda.
	-- Exception : Cle_Absente_Exception si Colonne n'est pas utilisée dans la Sda
	procedure Supprimer (Sda : in out T_LCA ; Colonne : in Integer);
	
	-- Supprimer tous les éléments d'une Sda.
	procedure Vider (Sda : in out T_LCA) with
		Post => Est_Vide (Sda);
				
	-- Initialise une matrice creuse avec une même coordonnée
	procedure Initialiser_Matrice_Creuse(M : in out T_Matrice_Creuse);

	-- Calcule et renvoie le produit d'un vecteur V et d'une matrice creuse M
	function Produit_Vecteur_Matrice_Creuse(V : in T_Vecteur; M : in T_Matrice_Creuse; alpha : in Float) return T_vecteur;

	-- Calcule et renvoie le produit d'un scalaire a et d'une matrice creuse M
	procedure Produit_Scalaire_Matrice_Creuse(a : in T_Element; M : in out T_Matrice_Creuse);

	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice creuse M
	function Somme_Ligne_Lca(M : in T_Matrice_Creuse; Ligne_i : in Integer) return T_Element;
		
	-- Normalise la ième ligne de M
	procedure Normaliser_Ligne_Lca(M : in out T_Matrice_Creuse; Ligne_i : in Integer);
		
	-- Supprimer tous les éléments d'une Matrice creuse.
	procedure Vider_Matrice (M : in out T_Matrice_Creuse);
		
end Algebre;
