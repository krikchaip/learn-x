/**
 * @template T
 * @typedef {Object} DataResponse
 * @property {'data'} type
 * @property {T} payload
 */

/**
 * @template {Record<String, any>} V
 * @param {V} value
 * @returns {DataResponse<V>}
 */
function data(value) {
  return {
    type: 'data',
    payload: value
  }
}

module.exports = data
