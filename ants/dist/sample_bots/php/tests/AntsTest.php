<?php
require_once 'PHPUnit/Framework.php';

require_once '../Ants.php';

class AntsTest extends PHPUnit_Framework_TestCase
{
    private $ants;

    protected function setUp()
    {
        $this->ants = new Ants();
    }

    public function testFinishTurn()
    {
        ob_start();
        $this->ants->finishTurn();
        $output = ob_get_clean();
        $this->assertEquals("go\n", $output);
    }

    public function testIssueOrder()
    {
        ob_start();
        $this->ants->issueOrder(2, 3, 'N');
        $output = ob_get_clean();
        $this->assertEquals("o 2 3 N\n", $output);
    }


    /**
     * @depends testFinishTurn
     * @depends testIssueOrder
     */
    public function testIssueOrderAndFinishTurn()
    {
        ob_start();
        $this->ants->issueOrder(2, 3, 'N');
        $this->ants->finishTurn();
        $output = ob_get_clean();
        $this->assertEquals("o 2 3 N\ngo\n", $output);
    }

    public function testSetup()
    {
        $data = explode("\n", "rows 10\ncols 15\nloadtime 2500\nturntime 1000\nturns 278\nviewradius2 64\nattackradius2 9\nspawnradius2 4\n");
        $this->ants->setup($data);

        $this->assertEquals(10, $this->ants->rows, "Unexpected rows value");
        $this->assertEquals(15, $this->ants->cols, "Unexpected cols value");
        $this->assertEquals(1000, $this->ants->turntime, "Unexpected turntime value");
        $this->assertEquals(2500, $this->ants->loadtime, "Unexpected loadtime value");
        $this->assertEquals(278, $this->ants->turns, "Unexpected turns value");
        $this->assertEquals(64, $this->ants->viewradius2, "Unexpected viewradius2 value");
        $this->assertEquals(9, $this->ants->attackradius2, "Unexpected attackradius2 value");
        $this->assertEquals(4, $this->ants->spawnradius2, "Unexpected spawnradius2 value");

        $this->assertType('array', $this->ants->map, 'Unexpected type of map');
        $this->assertEquals($this->ants->rows, count($this->ants->map), 'Number of rows in map doesn\'t match rows property');
        $this->assertEquals($this->ants->cols, count($this->ants->map[0]), 'Number of cols in map doesn\'t match cols property');
    
        $this->assertEquals(LAND, $this->ants->map[2][2], 'Initial type of map tile isn\'t LAND');
    }

    /**
     * @depends testSetup
     */
    public function testUpdate()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array(), $this->ants->myAnts, 'inital value of myAnts isn\'t empty array');
        $this->assertEquals(array(), $this->ants->enemyAnts, 'inital value of enemyAnts isn\'t empty array');
        $this->assertEquals(array(), $this->ants->deadAnts, 'inital value of deadAnts isn\'t empty array');
        $this->assertEquals(array(), $this->ants->food, 'inital value of food isn\'t empty array');

        $data = explode("\n", "a 2 2 0\na 8 8 1\nf 5 5");
        $this->ants->update($data);

        $this->assertEquals(array( array(2,2) ), $this->ants->myAnts, 'myAnts array wasn\t updated');
        $this->assertEquals(array( array(8,8) ), $this->ants->enemyAnts, 'enemyAnts array wasn\t updated');
        $this->assertEquals(array( array(5,5) ), $this->ants->food, 'food array wasn\t updated');
        $this->assertEquals( FOOD, $this->ants->map[5][5], 'map was not updated by first update');

        $data = explode("\n", "a 2 2 0\na 8 8 1\n");
        $this->ants->update($data);
        $this->assertEquals(array(), $this->ants->food, 'food array wasn\t updated by second update');
        $this->assertEquals( LAND, $this->ants->map[5][5], 'map was not updated by second update');

    }

    /**
     * @depends testUpdate
     */
    public function testPassable()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        //set water tiles
        $data = explode("\n", "a 2 2 0\nw 3 3\nw 3 4\nf 5 5\nd 4 5 1");
        $this->ants->update($data);

        $this->assertTrue( $this->ants->passable(1,1), 'Land is not passable');
        $this->assertTrue( $this->ants->passable(4,5), 'Dead ant is not passable');
        $this->assertTrue( $this->ants->passable(2,2), 'Ant is not passable');
        $this->assertTrue( $this->ants->passable(5,5), 'Food is passable');
        $this->assertFalse( $this->ants->passable(3,3), 'Water is passable');
    }
    
    /**
     * @depends testUpdate
     */
    public function testUnoccupied()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        //set water tiles
        $data = explode("\n", "a 2 2 0\nw 3 3\nw 3 4\nf 5 5\nd 4 5 1");
        $this->ants->update($data);

        $this->assertTrue( $this->ants->unoccupied(1,1), 'Land is occupied');
        $this->assertTrue( $this->ants->unoccupied(4,5), 'Dead ant is occupied');
        $this->assertTrue( $this->ants->unoccupied(5,5), 'Food is occupied');
        $this->assertFalse( $this->ants->unoccupied(2,2), 'Ant is unoccupied');
        $this->assertFalse( $this->ants->unoccupied(3,3), 'Water is unoccupied');
    }

    public function testDestinationBasic()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array(2,3), $this->ants->destination(3, 3, 'n'));
        $this->assertEquals(array(4,3), $this->ants->destination(3, 3, 's'));
        $this->assertEquals(array(3,4), $this->ants->destination(3, 3, 'e'));
        $this->assertEquals(array(3,2), $this->ants->destination(3, 3, 'w'));
    }

    /**
     * @depends testDestinationBasic
     */
    public function testDestinationWrap()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array(9,0), $this->ants->destination(0, 0, 'n'));
        $this->assertEquals(array(0,0), $this->ants->destination(9, 0, 's'));
        $this->assertEquals(array(5,0), $this->ants->destination(5, 14, 'e'));
        $this->assertEquals(array(5,14), $this->ants->destination(5, 0, 'w'));
    }

    public function testDistanceStraight()
    {
        $this->assertEquals(3, $this->ants->distance( 2, 2, 5, 2) );
        $this->assertEquals(4, $this->ants->distance( 2, 4, 6, 4) );
    }

    public function testDistanceDiagonal()
    {
        $this->assertEquals(sqrt(8), $this->ants->distance( 2, 2, 4, 4) );
        $this->assertThat($this->ants->distance( 2, 2, 4, 7), $this->equalTo(5.4, 0.1) );
    }

    public function testDistanceOverBorder()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(4, $this->ants->distance( 2, 2, 2, 13) );
        $this->assertThat($this->ants->distance( 0, 0, 9, 14), $this->equalTo(1.4, 0.1) );
    }

    public function testDirectionStraight()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array('n'), $this->ants->direction( 5, 7, 1, 7) );
        $this->assertEquals(array('s'), $this->ants->direction( 5, 7, 9, 7) );
        $this->assertEquals(array('e'), $this->ants->direction( 5, 7, 5, 12) );
        $this->assertEquals(array('w'), $this->ants->direction( 5, 7, 5, 2) );
    }

    public function testDirectionDiagonal()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array('n','e'), $this->ants->direction( 5, 7, 1, 10) );
        $this->assertEquals(array('s','e'), $this->ants->direction( 5, 7, 9, 10) );
        $this->assertEquals(array('s','w'), $this->ants->direction( 5, 7, 9, 2) );
        $this->assertEquals(array('n','w'), $this->ants->direction( 5, 7, 1, 2) );
    }

    public function testDirectionOverBorder()
    {
        $data = explode("\n", "rows 10\ncols 15\n");
        $this->ants->setup($data);

        $this->assertEquals(array('n'), $this->ants->direction( 2, 7, 8, 7) );
        $this->assertEquals(array('s'), $this->ants->direction( 8, 7, 1, 7) );
        $this->assertEquals(array('e'), $this->ants->direction( 5, 12, 5, 1) );
        $this->assertEquals(array('w'), $this->ants->direction( 5, 3, 5, 12) );
    }

}