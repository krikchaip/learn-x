/**
 * @typedef {Object} Acknowledge
 * @property {'ack'} type
 * @property {Object} payload
 * @property {String} payload.message
 */

/**
 * @param {String} message
 * @returns {Acknowledge}
 */
function ack(message) {
  return {
    type: 'ack',
    payload: { message }
  }
}

module.exports = ack
