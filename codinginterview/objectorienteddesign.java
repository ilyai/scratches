class ObjectOrientedDesign {
    static class Restaurant {
        private static Restaurant _instance;
        protected Restaurant() {}
        public static Restaurant getInstance() {
            if (_instance == null) {
                _instance = new Restaurant();
            }
            return _instance;
        }
    }

    enum GameType { Poker, BlackJack }

    static class CardGame {
        static class PokerGame extends CardGame {}
        static class BlackJackGame extends CardGame {}
        public static CardGame createCardGame(GameType type) {
            if (type == GameType.Poker) return new PokerGame();
            if (type == GameType.BlackJack) return new BlackJackGame();
            return null;
        }
    }

    public static void main(String[] args) {
        CardGame game = CardGame.createCardGame(GameType.Poker);
        System.out.println(game);
    }
}