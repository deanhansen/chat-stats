(function(){
  function typeTextFromElement(el, speed){
    if(!el) return;
    if(el.dataset.typing === '1') return;      // already typing
    const text = el.textContent || '';
    el.textContent = '';                       // clear then type
    el.dataset.typing = '1';
    let i = 0;
    (function step(){
      if(i < text.length){
        el.textContent += text.charAt(i++);
        setTimeout(step, speed);
      } else {
        el.dataset.typing = '0';
        el.dataset.typed = '1';
      }
    })();
  }

  function runTypingIfNeeded(){
    // only run if conditions are met
    if(Shiny && Shiny.shinyapp) {
      const inputs = Shiny.shinyapp.$inputValues;
      const outputs = Shiny.shinyapp.$values;

      if(inputs.selected_tab === 'Raw Data' && outputs.dataReady === true){
        document.querySelectorAll('.matrix-text.to-type').forEach(function(el){
          if (!el.dataset.typed) {
            const speed = parseInt(el.dataset.speed, 10) || 40;
            typeTextFromElement(el, speed);
          }
        });
      }
    }
  }

  // rerun whenever inputs or outputs change
  document.addEventListener('shiny:inputchanged', runTypingIfNeeded);
  document.addEventListener('shiny:value', runTypingIfNeeded);

})();
