<?php
require_once 'PHPUnit/Framework.php';
require_once '../HunterBot.php';

class HunterBotTest extends PHPUnit_Framework_TestCase
{
    private $bot;
    private $ants;

    protected function setUp()
    {
        $this->ants = new Ants();
        $this->bot = new HunterBot();
    }

    public function testAntGoesTowardsFood()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\nf 2 5");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 e\n", $output);
    }

    public function testAntGoesTowardsEnemy()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\na 5 2 1");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 s\n", $output);
    }

    /**
     * @depends testGoesTowardsFood
     */
    public function testTwoAntsGoTowardsDifferentFood()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $data = explode("\n", "a 2 2 0\na 4 2 0\nf 2 5\nf 7 2");
        $this->ants->update($data);

        ob_start();
        $this->bot->doTurn($this->ants);
        $output = ob_get_clean();

        $this->assertEquals("o 2 2 e\no 4 2 s\n", $output);
    }

    
}