from mpf.core.mode import Mode


class base(Mode):
    def __init__(self, machine, config: dict, name: str, path):
        super().__init__(machine, config, name, path)
        self.bonus_leds = {
            1000: self.machine.leds.ld_bonus_1k,
            2000: self.machine.leds.ld_bonus_2k,
            3000: self.machine.leds.ld_bonus_3k,
            4000: self.machine.leds.ld_bonus_4k,
            5000: self.machine.leds.ld_bonus_5k,
            6000: self.machine.leds.ld_bonus_6k,
            7000: self.machine.leds.ld_bonus_7k,
            8000: self.machine.leds.ld_bonus_8k,
            9000: self.machine.leds.ld_bonus_9k,
            10000: self.machine.leds.ld_bonus_10k,
            20000: self.machine.leds.ld_bonus_20k,
        }

    def mode_init(self):
        pass

    def mode_start(self, **kwargs):
        """register events"""
        self.add_mode_event_handler('increase_bonus', self.on_increase_bonus)
        self.add_mode_event_handler('ball_started', self.on_ball_started)
        self.add_mode_event_handler('mode_base_started', self.on_mode_base_started)
        self.add_mode_event_handler('player_bonus_score', self.on_player_bonus_score)

    def on_player_bonus_score(self, **kwargs):
        """called when the bonus_score variable is updated"""
        self.log.info('**** display bonus_score (player_bonus_score) ****')
        self.__update_leds()

    def on_mode_base_started(self, **kwargs):
        # TODO : never called ?
        self.log.info('**** display bonus_score (mode_base_started) ****')
        self.__update_leds()

    def on_ball_started(self, **kwargs):
        # TODO : never called ?
        self.log.info('**** display bonus_score (ball_started) ++reset++ ****')
        self.__update_leds()

    def on_increase_bonus(self, **kwargs):
        """event received on bonus shot hit"""
        self.log.info('**** display bonus_score (increase_bonus) ****')
        bonus_score = self.player['bonus_score']
        if bonus_score == 40000:
            self.machine.events.post('bonus_40k')
        elif bonus_score == 30000:
            self.machine.events.post('bonus_30k')
        elif bonus_score == 20000:
            self.machine.events.post('bonus_20k')
        elif bonus_score == 10000:
            self.machine.events.post('bonus_10k')
        self.__update_leds()

    def __update_leds(self):
        """
        ld_bonus_1k
        ld_bonus_2k
        ld_bonus_3k
        ld_bonus_4k
        ld_bonus_5k
        ld_bonus_6k
        ld_bonus_7k
        ld_bonus_8k
        ld_bonus_9k
        ld_bonus_10k
        ld_bonus_20k
        """
        score = self.player['bonus_score']
        bonus_score = score
        self.log.info('**** _update_leds (%s) ****' % score)

        if score >= 20000:
            self.machine.leds.ld_bonus_20k.on()
            score -= 20000
        else:
            self.machine.leds.ld_bonus_20k.off()

        if score >= 10000:
            self.machine.leds.ld_bonus_10k.on()
            score -= 10000
        else:
            self.machine.leds.ld_bonus_10k.off()

        try:
            for v, l in self.bonus_leds.items():
                if v < 10000:
                    if v == score:
                        l.on()
                    elif v <= score and bonus_score > 30000:
                        l.on()
                    else:
                        l.off()
        except KeyError:
            pass

