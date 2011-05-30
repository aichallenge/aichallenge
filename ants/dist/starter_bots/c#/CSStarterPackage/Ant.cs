using System;
using System.Collections.Generic;

namespace CSStarterPackage
{
    class Ant : Location
    {
        public Ant(int r, int c, int player)
            : base(r, c)
        {
            // set owner
            this.owner = player;
        }
        public int owner;
    }
}
