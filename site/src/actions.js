export const REQUEST_CONSUMERS = 'REQUEST_CONSUMERS'
export const RECEIVE_CONSUMERS = 'RECEIVE_CONSUMERS'
export const SELECT_CONSUMER = 'SELECT_CONSUMER'

export function fetchConsumersIfNeeded() {
  return (dispatch, getState) => {
    if (!getState().hasLoadedData) {
      return dispatch(fetchConsumers())
    }
  }
}

export const selectConsumer = (id) => {
  return {
    type: 'SELECT_CONSUMER',
    id
  }
}

function fetchConsumers() {
  return dispatch => {
    dispatch(requestConsumers())
    return fetch("http://localhost:8081/api/meatbars/consumption")
      .then(response => response.json())
      .then(json => dispatch(receiveConsumers(json)))
  }
}

function requestConsumers() {
  return {
    type: REQUEST_CONSUMERS
  }
}

function receiveConsumers(json) {
  return {
    type: RECEIVE_CONSUMERS,
    consumption: json,
    receivedAt: Date.now()
  }
}
