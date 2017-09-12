# Le dilemme entre code expressif et code générique FP

Utiliser au mieux la plomberie fournie par le langage de programmation, ou exprimer au mieux le domaine métier ? C'est le dilemme habituel dans un langage de programmation tel que Java.

Voyons cela de plus près au travers de quelques exemples, et comment nous parvenons à résoudre ce problème avec plus ou moins de bonheur selon le langage de programmation choisi.


## Type standard et type métier

Prenons l'exemple du prix et quantité en e-commerce. Nous souhaitons créer un type `Price` autour d'un *double* et un type `Quantity` autour d'un *int* afin d'exprimer au mieux le métier avec des types. Cela amène aussi la protection du typage, pour éviter de passer un `int` à la place d'un autre par erreur. 

En Java, il faut donc créer une classe pour chaque :

    public final class Price {
         private final double value;
     
         public Price(double value) { this.value = value; }
     
         ...
     }

    public final class Quantity {
         private final int value;
     
         public Quantity(int value) { this.value = value; }
     
         ...
     }

Maintenant nous voulons multiplier le prix par la quantité pour calculer le prix total, qui doit être aussi de type `Price`:

    public final class Price {
        ...
        public Price multiply(double multiplicand) {
          return new Price(this.value * multiplicand);
        }
     }
     
     public final class Quantity {
        ...
        public double asDouble() { return value; }
     }


Nous aimerions pouvoir passer directement une instance de `Quantity` en paramètre, mais cela nous oblige ou bien à sortir la valeur primitive :

    myPrice.multiply(myQuantity.asDouble());

Ou bien à modifier la méthode `multiply()` pour accepter le type `Quantity`, ce qui la rend désormais spécifique à ce type, et donc couplée par la même occasion :

     Price multiply(Quantity quantity) {
        return new Price(this.value * quantity.asDouble());
     }
     
Celà dit d'un point vue métier nous pouvons admettre que nous puissions multiplier un prix par une quantité.

En Haskell, la notion de synonyme ou alias permet de donner un nom plus métier au type :

     type Price = Double
     
     type Quantity = Int
     
Nous avons un type sur-mesure, mais que nous pouvons aussi utiliser comme un entier quand nous le souhaitons, car il reste aussi un entier :

    multiply :: Price -> cashregsiter.Quantity -> Price
    multiply p q = p * (asDouble q)
                 where asDouble = fromIntegral
                 
Notez qu'on peut appeler la fonction `multiply` avec n'importe quel entier.

    multiply 4.5 5 `shouldBe` 22.5 
    
    multiply 4.5 (6 :: Int) `shouldBe` 27.0
    
Si nous souhaitons faire des types wrapper comme en Java, nous utilisons des `newtype` :

    newtype Price = Price Double
        deriving (Eq, Show)

    newtype Quantity = Quantity Int
        deriving (Show)  

La fonction `multiply` s'appuie alors sur des "déconstructeurs" pour extraire les valeurs du prix et de la quantité :

    multiply :: Price -> Quantity -> Price
    multiply (Price p) (Quantity q) = Price (p * (asDouble q))
            where asDouble = fromIntegral
            
Une fois le calcul réalisé nous contruisons le prix avec le résultat. Pour appeler la fonction il faut créer un prix et une quantité :

    (Price 4.5) `multiply` (Quantity 5) `shouldBe` Price 22.5 
    
Là nous sommes donc vraiment "type-safe". De plus comme la fonction `multiply` a deux arguments, nous pouvons l'infixer. C'est plus naturel de dire "le prix multiplié par la quantité".
         
D'autres façons de contourner le problème d'être à la fois un type standard et un type sur-mesure métier sont la conversion implicite (implicit cast en C++, C# ou Scala). Java supporte une construction similaire, le "unboxing", mais seulement pour les types primitifs.   


## Monoid standard et monoid métier

Maintenant nous souhaitons additionner des prix. Nous définissons donc une méthode add() à la class Price :

    Price add(Price other) {
      return other != null ? new Price(value + other.value) : this;
    } 

Et par confort, j'ajoute le prix zero, bien utile dans de nombreux cas :

    public static final Price ZERO = new Price(0.);

Nous avons donc de fait un type qui obéit à la structure et aux propriétés d'un [Monoid](https://en.wikipedia.org/wiki/Monoid). Pas d'inquiétude si vous ne savez pas ce que c'est, ce n'est pas indispensable pour cet article ! 

Imaginons que nous avons une interface Monoid disponible que nous utilisons déjà un peu partout :

    public interface Monoid<M extends Monoid<M>> {

        M append(M other);

        M empty();

        default M concat(M... ms) {
            return Stream.of(ms).reduce(empty(), Monoid::append);
        }
    }

Nous souhaitons alors que Price implémente l'interface Monoid :

    class Price implements Monoid<Price> {
       ...
       public Price add(Price otherPrice) {
           return append(otherPrice);
       }
       
        public Price total(Price... prices) {
           return concat(prices);
        }
       
        @Override
        public Price append(Price other) {
           return other != null ? new Price(value + other.value) : this;
        }
       
        @Override
        public Price empty() {
            return ZERO;
        }
    }

Cela nous oblige à définir les méthodes obligatoires **génériques** append() et empty(), qui renvoient vers les membres **expressifs** du métier, la méthode add() et le champs ZERO. Cela va encombrer la classe.

En F# ou Haskell, il est possible là aussi de définir des **alias de fonctions**. Cela permet d'avoir une fonction nommée selon le domaine métier, tout en étant en même temps l'implémentation d'une fonction définie dans une "interface" sous un autre nom :

    import Data.Monoid
    
    instance Monoid Price where
        mempty = Price 0
        mappend (Price p1) (Price p2) = Price (p1 + p2)
    
    add :: Price -> Price -> Price
    add = mappend
    
    total :: [Price] -> Price
    total = mconcat

    (Price 10.0) `mappend` (Price 20.0) `shouldBe` Price 30.0

    (Price 10.0) `add` (Price 20.0) `shouldBe` Price 30.0

    mconcat [Price 12.0, Price 13.0, Price 14.0] `shouldBe` Price 39.0

    total [Price 12.0, Price 13.0, Price 14.0] `shouldBe` Price 39.0

## Predicate standard et predicate métier

Un dernier exemple pour la route, cette fois avec les `Predicate` Java 8 et une astuce !

Nous avons de nombreuses règles de dégressivité sur les quantités, que nous avons modélisées sous forme de critères :

    public interface QuantityCriteria {
       boolean isSatisfied(Quantity q);
    } 

Le nommage correspond au langage du métier dans notre domaine. Cependant, il est évident que notre critère n'est autre qu'un prédicat, et pour pouvoir utiliser toute la plomberie fournie par le langage de programmation autour des prédicats, il faudrait implémenter l'interface `Predicate` standard :

    public interface QuantityCriteria extends Predicate<Quantity>{
       boolean isSatisfied(Quantity q);
    } 

Ce qui oblige alors à définir en plus la méthode obligatoire test(), un petit encombrement :

    public interface QuantityCriteria extends Predicate<Quantity> {
       boolean isSatisfied(Quantity q);
       boolean test(Quantity q);
    } 

Il faut de plus que la méthode `test()` renvoie à la méthode `isSatisfied()`. Pour éviter de faire celà dans chaque implémentation concrète, nous pouvons utiliser une méthode *default* dans notre interface:

    public interface QuantityCriteria extends Predicate<Quantity> {
        boolean isSatisfied(Quantity q);
        default boolean test(Quantity q) { return isSatisfied(q); }
    }

Et voilà ! Nous pouvons donc désormais passer n'importe quelle instance de `QuantityCriteria` à toute fonction qui attend un `Predicate` en paramètre, c'est cool. 
    
Le `QuantityCriteria` peut également être implémenté par une lambda, notamment dans la classe `Quantity` pour accèder à sa valeur sans casser l'encapsulation :

    public final class Quantity {
        ...
        public static QuantityCriteria criteria(IntPredicate predicate) {
          return quantity -> predicate.test(quantity.value);
        }
    }

Imaginons maintenant que nous voulions composer plusieurs `QuantityCriteria` avec un AND logique :

    QuantityCriteria over10 = Quantity.criteria(v -> v > 10);
    QuantityCriteria below100 = Quantity.criteria(v -> v < 100);
    Predicate<Quantity> between10And100 = over10.and(below100);

Notez que la méthode `and` intégrée retourne un simple `Predicate`, et non une `QuantityCriteria`. Sniff ! Pourtant nous souhaiterions utiliser les critères composés, de type `Predicate<Quantity>`, dans la méthode `isEligible` qui attend un `QuantityCriteria`, idéalement :

    boolean eligible = myPurchaseHistory.isEligible(between10And100);

Et bien c'est impossible ! En contournement, nous pouvons enrichir `QuantityCriteria` d'une méthode `and` d'adaptation :

    public interface QuantityCriteria extends Predicate<Quantity> {
        boolean isSatisfied(Quantity q);
    
        default boolean test(Quantity q) { return isSatisfied(q); }
    
        default QuantityCriteria and(QuantityCriteria other) {
            return quantity -> Predicate.super.and(other).test(quantity);
        }
    }
    
Le problème avec cette approche est qu'il faudra ensuite redéfinir toutes les autres méthodes autour des `Predicate`, par exemple : `or`, `all`, `any`, `not`.

Mais tout n'est pas perdu. Par exemple nous pouvons utiliser une méthode référence (équivalent à une lambda) :

    myHistoryPurshase.isEligible(between10And100::test);

Ou encore enrichir `QuantityCriteria` avec un adapter :

    public interface QuantityCriteria extends Predicate<Quantity> {
        boolean isSatisfied(Quantity q);

        default boolean test(Quantity q) {
            return isSatisfied(q);
        }

        static QuantityCriteria from(Predicate<Quantity> predicate) {
            return predicate::test;
        }
    }

Ce qui permet ensuite de convertir à la volée un Predicate générique dans son équivalent typé sur-mesure métier :

    myHistoryPurshase.isEligible(QuantityCriteria.from(between10And100));

Notez en bonus que `QuantityCriteria` n'a qu'une seule méthode abstraite, et est donc une `FunctionalInterface`. Il est donc possible de passer une instance de `QuantityCriteria` partout où une lambda de même signature est attendue :

    myStream.filter(myQuantityCritera);
    
Dans les cercles d'amateurs de programmation fonctionnelle, il existe bien souvent un biais très fort pour le générique, au détriment parfois de l'expressivité du langage et des concepts métiers. Ce n'est pas une fatalité. 

Et si ça devenait un jeu, d'exprimer le métier le plus litérallement possible, dans ses termes propres, tout en gardant tous les avantages propres aux langages et leurs écosystèmes disponibles ?
