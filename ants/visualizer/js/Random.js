/**
 * Random provides functions to generate random numbers in integer ranges.
 */
Random = {
	/**
	 * returns an integer in the range [0..range[
	 * @param {number} range the exclusive limit of the range
	 */
	range: function(range) {
		return Math.random() * range | 0;
	},
	/**
	 * returns an integer in the range [from..to]
	 * @param {number} from the low value of the range (inclusive)
	 * @param {number} to the high value of the range (inclusive)
	 */
	fromTo: function(from, to) {
		return from + (Math.random() * (1 + to - from) | 0);
	}
};
