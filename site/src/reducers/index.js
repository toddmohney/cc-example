import { REQUEST_CONSUMERS, RECEIVE_CONSUMERS } from '../actions'

const meatbarApp = (state, action) => {
  switch (action.type) {
    case RECEIVE_CONSUMERS:
      return transformPayload(state, action);

    case REQUEST_CONSUMERS:
    default:
      return state
  }
}

function transformPayload(state, action) {
  let consumedMeatbars = action.consumers;
  let meatbarsByEater = groupByEater(consumedMeatbars)

  return {
    meatbarsByEater,
    consumedMeatbars,
    hasLoaded: true
  }
}

function groupByEater(consumers) {
  return consumers.reduce(function(acc, consumer) {
    let key = consumer.eater.id;
    if(acc[key] == undefined) {
      acc[key] = [];
    }
    acc[key].push(consumer);
    return acc;
  }, {});
}

export default meatbarApp

